open Alcotest
open Kxclib_js

module Rfc8785 = struct
  external canonicalize : 'a -> string = "canonicalize_rfc_8785"
end

open Rfc8785

let test_canonicalize =
  let open QCheck2 in
  [
    Test.make ~count:2000 ~name:"Json_jcsnafi canonicalize"
      Jcsnafi_qcheck_generators.gen_jv_jcsnafi
      ~print:(fun jv ->
        let esc s = "\"" ^ String.escaped s ^ (
                        if String.escaped s <> s
                        then "\"mlesc" else "\"")
        in
        let rec show_jv : Json.jv -> string = function
          (* | `null -> "null" *)
          (* | `bool true -> "true" *)
          (* | `bool false -> "false" *)
          | (`null | `bool _ | `num _) as j -> Json.show j
          | `str s -> esc s
          | `arr xs -> "[" ^ (String.concat","(xs |&> show_jv)) ^ "]"
          | `obj fs -> "[" ^ (String.concat","(fs |&> fun (k, v) -> esc k ^ ":" ^ show_jv v)) ^ "]"
        in
        sprintf
          "input jv:\n\t%s\n\nRFC8785.canonicalize:\n\t%s\n\t%s\n\nJson_JCSnafi.unparse_jcsnafi:\n\t%s\n\t%s\n\nre-interp:\n\nRFC8785.canonicalize:\n\t%s\n\nJson_JCSnafi.unparse_jcsnafi:\n\t%s\n\n"

          (show_jv jv)

          (canonicalize & Json_ext.to_xjv jv)
          ((canonicalize & Json_ext.to_xjv jv) |> esc)

          (Json_JCSnafi.unparse_jcsnafi jv)
          (Json_JCSnafi.unparse_jcsnafi jv |> esc)

          (Json_ext.of_json_string_opt (canonicalize & Json_ext.to_xjv jv) >? show_jv |? "(invalid-json)")
          (Json_ext.of_json_string_opt (Json_JCSnafi.unparse_jcsnafi jv) >? show_jv |? "(invalid-json)"))
      (fun jv ->
        let canonicalized = canonicalize & Json_ext.to_xjv jv in
        let jcsnafi_unparsed = Json_JCSnafi.unparse_jcsnafi jv in
        String.equal canonicalized jcsnafi_unparsed)
  ] |&> QCheck_alcotest.to_alcotest

let () =
  Printexc.record_backtrace true;
  run "Baselib_json_csnafi_rfc_8785_test" ([
    "check external", [
      test_case "canonicalize" `Quick (fun () ->
        check' string
          ~msg:"It will be success to call external function 'canonicalize'"
          ~expected:"null"
          ~actual:(canonicalize Js_of_ocaml.Js.null)
        )
    ];
    "canonicalize", test_canonicalize
  ])
