open Kxclib
open Ppxlib

let rule_panic =
  let ext =
    Extension.
    (declare
       "panic"
       Context.expression
       Ast_pattern.(alt_option (single_expr_payload (estring __)) (pstr nil))
       (fun ~loc ~path:_ ->
         let (module B) = Ast_builder.make loc in
         function
         | Some hints ->
            [%expr failwith (Format.sprintf "panic at %s: %s" __LOC__
                               [%e B.estring ("noimpl: "^hints)])]
         | None -> [%expr failwith (Format.sprintf "panic at %s" __LOC__)]))
  in Context_free.Rule.extension ext

let rule_debug =
  let ext =
    Extension.
    (declare
       "debug"
       Context.expression
       Ast_pattern.(alt_option (single_expr_payload (estring __)) (pstr nil))
       (fun ~loc ~path:_ label ->
         let (module B) = Ast_builder.make loc in
         let label = match label with
           | Some label -> label | None -> __FILE__ in
         B.([%expr Format.(
           fun fmt ->
           let ppf = err_formatter in
           fprintf ppf "[DEBUG:%s] " [%e estring label];
           kfprintf (fun ppf ->
               pp_print_newline ppf();
               pp_print_flush ppf ())
             ppf fmt
         )])))
  in Context_free.Rule.extension ext

let rule_noimplval =
  let ext =
    Extension.
    (declare
       "noimplval"
       Context.expression
       Ast_pattern.(alt_option (single_expr_payload (estring __)) (pstr nil))
       (fun ~loc ~path:_ ->
         let (module B) = Ast_builder.make loc in
         function
         | Some hints ->
            [%expr failwith [%e B.estring ("noimpl: "^hints)]]
         | None -> [%expr failwith "noimpl"]))
  in Context_free.Rule.extension ext

let rule_noimplfunc =
  let ext =
    Extension.
    (declare
       "noimplfun"
       Context.expression
       Ast_pattern.(alt_option (single_expr_payload (estring __)) (pstr nil))
       (fun ~loc ~path:_ ->
         let (module B) = Ast_builder.make loc in
         function
         | Some hints ->
            [%expr fun _ -> failwith [%e B.estring ("noimpl: "^hints)]]
         | None -> [%expr fun _ -> failwith "noimpl"]))
  in Context_free.Rule.extension ext

let transformer_include : structure -> structure = fun str ->
  let module WorkingOCamlVersion = Migrate_parsetree.OCaml_410 in
  let module PpxlibOCamlVersion = Ppxlib_ast.Selected_ast in
  let module Conv = Migrate_parsetree.Convert
                      (PpxlibOCamlVersion)
                      (WorkingOCamlVersion) in
  let module Rev = Migrate_parsetree.Convert
                     (WorkingOCamlVersion)
                     (PpxlibOCamlVersion) in
  let str = Conv.copy_structure str in
  let open WorkingOCamlVersion.Ast.Parsetree in
  let process_stri = fun stri ->
    let _loc = stri.pstr_loc in
    let open Ppxlib.Parsetree in
    let str = Rev.copy_structure [stri] in
    let on_error = fun () -> [stri] in
    Ast_pattern.(
      (parse ~on_error
         ((pstr_attribute
             (attribute
                ~name:(string "include")
                ~payload:(single_expr_payload (estring __))))
          ^:: nil)
         _loc str
       (fun original ->
         let dir = _loc.loc_start.pos_fname |> Filename.dirname in
         let filename = Filename.concat dir original in
         let buf = Lexing.from_channel (open_in filename) in
         let parsed = Ppxlib.Parse.implementation buf in
         let (module B) = Ast_builder.make _loc in
         let attr label payload =
           B.(attribute ~name:(Located.mk label) ~payload |> pstr_attribute) in
         let _filename= B.(PStr [%str { original    = [%e (estring original)] ;
                                        transformed = [%e (estring filename)] ; }]) in
         let built =
           [attr "including.starts" _filename]
           @ parsed
           @ [attr "including.ends" _filename]
         in Conv.copy_structure built)))
  in
  let rec expander ({ pstr_desc; _ } as stri) =
    let process_mb mb : module_binding =
      let rec process_mexpr mexpr = match mexpr.pmod_desc with
        | Pmod_functor (mty, mexpr) ->
           { mexpr with
             pmod_desc =
               Pmod_functor (mty, process_mexpr mexpr)}
        | Pmod_structure str ->
           { mexpr with
             pmod_desc = Pmod_structure (List.fmap expander str)}
        | _ -> mexpr
      in { mb with pmb_expr = process_mexpr mb.pmb_expr } in
    match pstr_desc with
    | Pstr_module mb ->
       [ { stri with pstr_desc =
                       Pstr_module (process_mb mb) } ]
    | Pstr_recmodule mbs ->
       [ { stri with pstr_desc =
                       Pstr_recmodule
                         (List.map process_mb mbs) } ]
    | _ -> process_stri stri in
    str |> List.fmap expander |> Rev.copy_structure

let () =
  Driver.register_transformation
    ~rules:
    [rule_noimplval; rule_noimplfunc; rule_panic; rule_debug]
    ~impl:transformer_include
    "kxclib_ppx_transformation"
