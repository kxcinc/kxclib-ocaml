open Ppxlib
open Kxclib

let mapper =
  let transform_structure ~ctxt:_ _self (stru : Parsetree.structure) =
    stru |&> (fun ({pstr_desc; _} as item) ->
      match pstr_desc with
      | Pstr_type (rec_flag, decls) ->
         let decls =
           decls |&> (fun ({ ptype_attributes; _ } as decl) ->
             let ptype_attributes = ptype_attributes |?> (fun { attr_name; _ } ->
                 attr_name.txt <> "deriving") in
             { decl with ptype_attributes }
           )
         in
         { item with pstr_desc = Parsetree.Pstr_type (rec_flag, decls) }
      | _ -> item
    )
  in
  object (self)
    inherit
      [Expansion_context.Base.t] Ppxlib.Ast_traverse.map_with_context as super

    method! structure ctxt stru =
      super#structure ctxt (transform_structure ~ctxt self stru)
  end

let () =
  Driver.V2.register_transformation "kxclib_melange_ppx"
    ~impl:(fun ctxt str -> mapper#structure ctxt str)
    ~intf:(fun ctxt sig_ -> mapper#signature ctxt sig_);
  Driver.standalone()
