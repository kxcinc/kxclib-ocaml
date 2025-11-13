open Alcotest
open Kxclib_js
open Jsoo_test_helpers

module Rfc8785 = struct

  (* utf8 json string -> utf8 json string *)
  external canonicalize : bytes -> bytes = "canonicalize_rfc_8785"

  let canonicalize_jv jv =
    (* NB: seems that Js_of_ocaml.Js.string is buggy *)
    try
      Json.to_yojson jv
      |> Yojson.Safe.to_string
      |> Bytes.of_string
      |> canonicalize
    with e ->
      failwith' "%s: %s" (Json.show jv) (Printexc.to_string e)
end

open Rfc8785

let to_bytes = Bytes.of_string
let of_bytes = Bytes.to_string

let test_canonicalize =
  let open QCheck2 in
  [
    Test.make ~count:2000 ~name:"Json_jcsnafi canonicalize"
      QCheck2.Gen.(
      oneof [
          Jcsnafi_qcheck_generators.gen_jv_jcsnafi;

          (* regression: seems that Js_of_ocaml.Js.string is buggy *)
          return (`obj ["\237\159\191", `null]) ])
      ~print:(fun jv ->
        let canonicalized = canonicalize_jv jv |> of_bytes in
        let unparsed = Json_JCSnafi.unparse_jcsnafi jv in
        sprintf
          "input jv:\n\t%s\n\n"
          (show_jv jv)
        ^ sprintf "Rfc8785.canonicalize:\n\t%s\n\t%s\n\n"
          canonicalized
          (canonicalized |> esc)
        ^ sprintf "Json_JCSnafi.unparse_jcsnafi:\n\t%s\n\t%s\n\n"
          unparsed
          (unparsed |> esc)
        ^ sprintf "re-interp:\n\nRfc8785.canonicalize:\n\t%s\n\nJson_JCSnafi.unparse_jcsnafi:\n\t%s\n\n"
          (Json_ext.of_json_string_opt canonicalized >? show_jv |? "(invalid-json)")
          (Json_ext.of_json_string_opt unparsed >? show_jv |? "(invalid-json)"))
      (fun jv ->
        let canonicalized = canonicalize_jv jv in
        let jcsnafi_unparsed = Json_JCSnafi.unparse_jcsnafi jv |> to_bytes in
        Bytes.equal canonicalized jcsnafi_unparsed)
  ] |&> QCheck_alcotest.to_alcotest

let () =
  Printexc.record_backtrace true;
  run "Baselib_json_jcsnafi_rfc_8785_test" ([
    "check external", [
      test_case "canonicalize" `Quick (fun () ->
        check' bytes
          ~msg:"It will be success to call external function 'canonicalize'"
          ~expected:("null" |> to_bytes)
          ~actual:(canonicalize & Bytes.of_string "null")
        )
    ];
    "canonicalize", test_canonicalize
  ])
