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

let () =
  Driver.register_transformation ~rules:
    [rule_noimplval; rule_noimplfunc]
    "kxclib_ppx_transformation"
