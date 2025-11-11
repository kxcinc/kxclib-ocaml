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
      (Jcsnafi_qcheck_generators.gen_jv_jcsnafi
        |> Gen.map (fun jv ->
          (canonicalize & Json_ext.to_xjv jv, Json_JCSnafi.unparse_jcsnafi jv)
        ))
      ~print:(fun (canonicalized, unparsed) ->
        sprintf "RFC8785: %s, Json_JCSnafi: %s" canonicalized unparsed)
      (!!String.equal)
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
