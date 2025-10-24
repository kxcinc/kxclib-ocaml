let min_fi_int = (- (1 lsl 52)) ;;
let max_fi_int = (1 lsl 52) - 1 ;;

let invalid_neg_int_range = (QCheck2.Gen.int_range min_int (pred min_fi_int))
let invalid_pos_int_range = (QCheck2.Gen.int_range (succ max_fi_int) max_int)

let gen_random_byte_string =
  QCheck2.Gen.map String.of_list (QCheck2.Gen.list_size (QCheck2.Gen.int_range 1 100) QCheck2.Gen.char)

let is_invalid_utf8 str =
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

let is_exn exn (f : unit -> 'a) : bool =
  try
    ignore (f ());
    false
  with
  | e when e = exn -> true
  | _ -> false

let is_exn_p (p : exn -> bool) (f : unit -> 'a) : bool =
  try
    ignore (f ());
    false
  with
  | e -> p e

let is_invalid_arg_prefix_invalid_unicode = function
  | Invalid_argument msg -> String.starts_with "Invalid Unicode:" msg
  | _ -> false

let () =
  let that ?(count=200) name =
    QCheck2.Test.make
      ~name ~count in

  let run_tests tests = QCheck_base_runner.run_tests_main tests in
  [
    that "is_encodable_num: Invalid negative integer range" invalid_neg_int_range ~print:string_of_int
      (fun i ->
        float_of_int i |> Json_JCSnafi.is_encodable_num |> not
      );
    that "is_encodable_num: Invalid positive integer range" invalid_pos_int_range ~print:string_of_int
      (fun i ->
        float_of_int i |> Json_JCSnafi.is_encodable_num |> not
      );

    that "unparse_jcsnafi: Invalid negative integer range" invalid_neg_int_range ~print:string_of_int
      (fun i ->
        is_exn (Invalid_argument "float or out-of-range integer")
          (fun () -> Json_JCSnafi.unparse_jcsnafi(`num (float_of_int i)))
      );
    that "unparse_jcsnafi: Invalid positive integer range" invalid_pos_int_range ~print:string_of_int
      (fun i ->
        is_exn (Invalid_argument "float or out-of-range integer")
          (fun () -> Json_JCSnafi.unparse_jcsnafi(`num (float_of_int i)))
      );
    that "unparse_jcsnafi: Invalid UTF-8 string" gen_random_byte_string ~print:identity
      (fun s ->
        QCheck2.assume (is_invalid_utf8 s);
        is_exn_p is_invalid_arg_prefix_invalid_unicode
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`str s)));
    that "unparse_jcsnafi: Invalid UTF-8 object property name" gen_random_byte_string ~print:identity
      (fun s ->
        QCheck2.assume (is_invalid_utf8 s);
        is_exn_p is_invalid_arg_prefix_invalid_unicode
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`obj [(s, `null)])));
          (* TODO JSON.jv generator*)
  ] |> run_tests |> exit
