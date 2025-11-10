open Kxclib
open Kxclib_js
open Utils
open Jest

module SP = struct
  let succ f = f +. 1.0
  let pred f = f -. 1.0
end
  
let test_unparse_jcsnafi () =
  let case jv fmt = fun () ->
    let unparsed = Json_JCSnafi.unparse_jcsnafi jv in
    to_be (expect unparsed) fmt;
    Promise_io.return ()
  in

  let case_exn jv (exn: exn) = fun () ->
    let error_name = match exn with
      | Invalid_argument _ -> "Invalid_argument"
      | _ -> "Error"
    in
    to_throw (expect (fun () -> Json_JCSnafi.unparse_jcsnafi jv)) error_name;
    Promise_io.return ()
  in

  Jcsnafi_test_data.cases_for_unparse_jcsnafi (module SP) case case_exn
  |> fold_promise

let test_is_encodable () =
  let case jv (expected: bool) = fun () ->
    to_be (expect (Json_JCSnafi.is_encodable jv)) expected;
    Promise_io.return ()
  in
  
  Jcsnafi_test_data.cases_for_is_encodable (module SP) case
  |> fold_promise

let test_is_encodable_str () =
  let case s (expected: bool) = fun () ->
    to_be (expect (Json_JCSnafi.is_encodable_str s)) expected;
    Promise_io.return ()
  in
  Jcsnafi_test_data.cases_for_is_encodable_str case
  |> fold_promise

let test_is_encodable_num () =
  let case f (expected: bool) = fun () ->
    to_be (expect (Json_JCSnafi.is_encodable_num f)) expected;
    Promise_io.return ()
  in
  Jcsnafi_test_data.cases_for_is_encodable_num (module SP) case
  |> fold_promise

let test_compare_field_name () =
  let case str1 str2 (expected: int) = fun () ->
    to_be (expect (Json_JCSnafi.compare_field_name str1 str2)) expected;
    Promise_io.return ()
  in
  Jcsnafi_test_data.cases_for_compare_field_name case
  |> fold_promise

let test_compare_field_name_rfc8785 () =
  Jcsnafi_test_data.test_compare_field_name_rfc8785 &
    (fun ~actual ~expected () ->
      to_strict_equal (expect actual) expected;
      Promise_io.return ())
