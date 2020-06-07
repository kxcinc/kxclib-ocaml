open Kxclib
open Ppxlib

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

let transformer_include str =
  let open Ppxlib.Parsetree in
  let process_stri = function
    | { pstr_desc = Pstr_attribute ({ txt = "include"; _ }, payload);
        pstr_loc = _loc; } ->
       let dir = _loc.loc_start.pos_fname |> Filename.dirname in
       Ast_pattern.(
        (parse
           ~on_error:(fun _ -> failwith __LOC__)
           (single_expr_payload (estring __))
           _loc payload)
          (fun original ->
            let filename = Filename.concat dir original in
            let buf = Lexing.from_channel (open_in filename) in
            let parsed = Ocaml_common.Parse.implementation buf in
            let module Conv = Migrate_parsetree.Convert
                                (Migrate_parsetree.OCaml_current)
                                (Ppxlib.Import_for_core.Ocaml) in
            let (module B) = Ast_builder.make _loc in
            let str = Conv.copy_structure parsed in
            let attr label payload : Parsetree.structure_item =
              ({ txt = label; loc = _loc }, payload) |> B.pstr_attribute in
            let _filename : payload =
              B.(PStr
                   [%str
                    { original    = [%e (estring original)] ;
                      transformed = [%e (estring filename)] ; }]) in
            [attr "including.starts" _filename]
            @ str
            @ [attr "including.ends" _filename]))
    | x -> [x]
  in
  let rec expander ({ pstr_desc; _ } as stri) =
    let process_mb mb : module_binding =
      let rec process_mexpr mexpr = match mexpr.pmod_desc with
        | Pmod_functor (loc, mty, mexpr) ->
           { mexpr with
             pmod_desc =
               Pmod_functor (loc, mty, process_mexpr mexpr)}
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
  str |> List.fmap expander

let () =
  Driver.register_transformation
    ~rules:
    [rule_noimplval; rule_noimplfunc]
    ~impl:transformer_include
    "kxclib_ppx_transformation"
