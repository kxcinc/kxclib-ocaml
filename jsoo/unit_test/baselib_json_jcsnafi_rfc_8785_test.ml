open Alcotest
open Kxclib_js

module Rfc8785 = struct
  external canonicalize : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> bytes = "canonicalize_rfc_8785"

  let canonicalize_jv jv =
    try
      Json.to_yojson jv
      |> Yojson.Safe.to_string
      |> Js_of_ocaml.Js.string
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
        let canonicalized = canonicalize_jv jv |> of_bytes in
        let unparsed = Json_JCSnafi.unparse_jcsnafi jv in
        sprintf
          "input jv:\n\t%s\n\nRFC8785.canonicalize:\n\t%s\n\t%s\n\nJson_JCSnafi.unparse_jcsnafi:\n\t%s\n\t%s\n\nre-interp:\n\nRFC8785.canonicalize:\n\t%s\n\nJson_JCSnafi.unparse_jcsnafi:\n\t%s\n\n"

          (show_jv jv)

          canonicalized
          (canonicalized |> esc)

          unparsed
          (unparsed |> esc)

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
          ~actual:(canonicalize & Js_of_ocaml.Js.string "null")
        )
    ];
    "canonicalize", test_canonicalize
  ])
