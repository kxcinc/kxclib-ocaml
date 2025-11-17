open Kxclib_priv_test_lib.Json
open Kxclib.Json
open Jcsnafi_qcheck_generators

(* Helper functions *)

let does_throw exn (f : unit -> 'a) : bool =
  try
    ignore (f ());
    false
  with
  | e when e = exn -> true
  | _ -> false

let does_throw_p (p : exn -> bool) (f : unit -> 'a) : bool =
  try
    ignore (f ());
    false
  with
  | e -> p e

let does_throw_any (f : unit -> 'a) : bool =
  try
    ignore (f ());
    false
  with
  | _  -> true

let is_invalid_utf8 str : bool =
  let str_len = String.length str in
  let rec loop i =
    if i >= str_len then false
    else
      let utf8dec = String.get_utf_8_uchar str i in
        if Uchar.utf_decode_is_valid utf8dec then
          loop (i + Uchar.utf_decode_length utf8dec)
        else
          true
  in
    loop 0

let is_invalid_arg_prefix prefix : exn -> bool = function
  | Invalid_argument msg -> String.starts_with prefix msg
  | _ -> false

let is_whitespace : char -> bool = function
 | ' ' | '\t' | '\n' | '\r'  -> true
 | _ -> false

let count_preceding_backslashes s i : int =
  let rec count j cnt = 
    if j < 0 || s.[j] <> '\\' then cnt
    else count (j - 1) (cnt + 1)
in
  count (i - 1) 0

let has_extra_whitespace_outside_string s : bool =
  let len = String.length s in
  let rec loop i in_string = 
    if i >= len then false
    else
      let c = s.[i] in
      if c = '"' then
        let escape_count = count_preceding_backslashes s i in
        let in_string' = if escape_count mod 2 = 0 then not in_string
                         else in_string
        in
          loop (i + 1) in_string'
      else if not in_string && is_whitespace c then
        true
      else
        loop (i + 1) in_string
  in
    loop 0 false

(* Properties *)

let () =
  let that ?(count=200) name =
    QCheck2.Test.make
      ~name ~count in

  let run_tests tests = QCheck_base_runner.run_tests_main tests in
  [
    that "is_encodable_num: Invalid negative integer range" invalid_neg_fi_int64 ~print:Int64.to_string
      (fun i ->
        Int64.to_float i |> Json_JCSnafi.is_encodable_num |> not
      );
    that "is_encodable_num: Invalid positive integer range" invalid_pos_fi_int64 ~print:Int64.to_string
      (fun i ->
        Int64.to_float i |> Json_JCSnafi.is_encodable_num |> not
      );

    that "compare_field_name: " (QCheck2.Gen.pair gen_unicode_string gen_unicode_string)
     (fun (s1, s2) -> 
        let ret = Json_JCSnafi.compare_field_name s1 s2 in
        let flip_ret = Json_JCSnafi.compare_field_name s2 s1 in
        ret = (- flip_ret)
        );
    
    that "unparse_jcsnafi: No extra space is included" gen_jv_jcsnafi ~print:string_of_jv
      (fun jv ->
         let unparsed = Json_JCSnafi.unparse_jcsnafi jv in
         not (has_extra_whitespace_outside_string unparsed));
    that "unparse_jcsnafi: shuffled object elements" gen_jv_and_shuffled_pair ~print:(QCheck2.Print.pair string_of_jv string_of_jv)
      (fun (jv, shuffled_jv) -> 
        QCheck2.assume (jv <> shuffled_jv);
        let unparsed_jv = Json_JCSnafi.unparse_jcsnafi jv in
        let unparsed_shuffled_jv = Json_JCSnafi.unparse_jcsnafi shuffled_jv in
        unparsed_jv = unparsed_shuffled_jv);
    that "unparse_jcsnafi: Invalid negative integer range" invalid_neg_fi_int64 ~print:Int64.to_string
      (fun i ->
        does_throw_p (is_invalid_arg_prefix "Number cannot be safely encoded with Json_JCSnafi (encountering:")
          (fun () -> Json_JCSnafi.unparse_jcsnafi(`num (Int64.to_float i)))
      );
    that "unparse_jcsnafi: Invalid positive integer range" invalid_pos_fi_int64 ~print:Int64.to_string
      (fun i ->
        does_throw_p (is_invalid_arg_prefix "Number cannot be safely encoded with Json_JCSnafi (encountering:")
          (fun () -> Json_JCSnafi.unparse_jcsnafi(`num (Int64.to_float i)))
      );
    that "unparse_jcsnafi: Invalid UTF-8 string" gen_unicode_string_with_invalid_unicode_string ~print:identity
      (fun s ->
        does_throw_p (is_invalid_arg_prefix "Invalid Unicode:")
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`str s)));
    that "unparse_jcsnafi: Invalid UTF-8 object property name or string value" gen_jv_with_invalid_unicode ~print:string_of_jv
      (fun jv ->
        does_throw_p (is_invalid_arg_prefix "Invalid Unicode:")
          (fun () -> Json_JCSnafi.unparse_jcsnafi jv));
    that "unparse_jcsnafi: Unicode surrogate codepoint range" gen_unicode_string_with_surrogate ~print:identity
      (fun s ->
        does_throw_p (is_invalid_arg_prefix "Invalid Unicode:")
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`str s)));
    that "unparse_jcsnafi: Unicode surrogate codepoint range in object property name or string value" gen_jv_with_surrogates ~print:string_of_jv
      (fun jv ->
        does_throw_p (is_invalid_arg_prefix "Invalid Unicode:")
          (fun () -> Json_JCSnafi.unparse_jcsnafi jv));
    that "unparse_jcsnafi: Duplicated property names in object" gen_jv_with_duplicate_keys ~print:string_of_jv
      (fun jv ->
        does_throw_p (is_invalid_arg_prefix "Duplicate property names:")
          (fun () -> Json_JCSnafi.unparse_jcsnafi jv)
        );

    that "if `is_encodable_num` is true, it can be unparsed into JCSnafi " gen_int64 ~print:Int64.to_string
      (fun i ->
        let f = Int64.to_float i in
        let jv_num = `num f in
        if (Json_JCSnafi.is_encodable_num f) then
          (ignore (Json_JCSnafi.unparse_jcsnafi jv_num); true)
        else
          (does_throw_p (is_invalid_arg_prefix "Number cannot be safely encoded with Json_JCSnafi (encountering:")
                        (fun () -> Json_JCSnafi.unparse_jcsnafi jv_num))
      );
    that "if `is_encodable_str` is true, it can be unparsed into JCSnafi " gen_valid_or_invalid_unicode_string ~print:identity
      (fun s ->
        let jv_str = `str s in
        if (Json_JCSnafi.is_encodable_str s) then
          (ignore (Json_JCSnafi.unparse_jcsnafi jv_str); true)
        else
          (does_throw_p (is_invalid_arg_prefix "Invalid Unicode:")
                        (fun () -> Json_JCSnafi.unparse_jcsnafi jv_str))
      );
    that "if `is_encodable` is true, it can be unparsed into JCSnafi " gen_valid_or_invalid_jv ~print:string_of_jv
      (fun jv ->
        if (Json_JCSnafi.is_encodable jv) then
          (ignore (Json_JCSnafi.unparse_jcsnafi jv); true)
        else
          (does_throw_any (fun () -> Json_JCSnafi.unparse_jcsnafi jv))
      );
  ] |> run_tests |> exit
