open Kxclib
open Kxclib_js
open Utils
open Jest

(* test_data/jcsnafi_test_data.ml *)
module Jcsnafi_data = Jcsnafi_test_data.Make(struct
  let succ f = f +. 1.0
  let pred f = f -. 1.0
end)

open struct
  let case actual (expected) = fun () ->
    to_be (expect actual) expected;
    Promise_io.return ()
end

let test_unparse_jcsnafi () =
  let case_exn f (exn: exn) = fun () ->
    let error_name = match exn with
      | Invalid_argument _ -> "Invalid_argument"
      | _ -> "Error"
    in
    to_throw (expect (f ())) error_name;
    Promise_io.return ()
  in

  Jcsnafi_data.cases_for_unparse_jcsnafi case case_exn
  |> fold_promise

let test_is_encodable () =
  Jcsnafi_data.cases_for_is_encodable case
  |> fold_promise

let test_is_encodable_str () =
  Jcsnafi_data.cases_for_is_encodable_str case
  |> fold_promise

let test_is_encodable_num () =
  Jcsnafi_data.cases_for_is_encodable_num case
  |> fold_promise

let test_compare_field_name () =
  Jcsnafi_data.cases_for_compare_field_name case
  |> fold_promise

let test_compare_field_name_rfc8785 () =
  Jcsnafi_data.test_compare_field_name_rfc8785 &
    (fun ~actual ~expected () ->
      to_strict_equal (expect actual) expected;
      Promise_io.return ())
