module G = QCheck2.Gen

let min_fi_int = (- (1 lsl 52))
let max_fi_int = (1 lsl 52) - 1

(* Generators *)

let invalid_neg_fi_int_range = (QCheck2.Gen.int_range min_int (pred min_fi_int))
let invalid_pos_fi_int_range = (QCheck2.Gen.int_range (succ max_fi_int) max_int)

let gen_random_byte_string =
  QCheck2.Gen.(map String.of_list (list_size (int_range 1 100) QCheck2.Gen.char))

let gen_unicode_char =
  QCheck2.Gen.(
    let gen_unicode_codepoint = oneof [int_range 0x0000 0xD7FF; int_range 0xE000 0x10FFFF] in
    map Uchar.of_int gen_unicode_codepoint
  )

let gen_lone_surrogate_string =
  let gen_surrogate_codepoint = QCheck2.Gen.int_range 0xD800 0xDFFF in
  let bytes_of_surrogate_codepoint code =
    (* | 1110xxxx | ________ | ________ | *)
    let b1 = Char.chr (0xE0 lor (code lsr 12)) in
    (* | ________ | 10xxxxxx | ________ | *)
    let b2 = Char.chr (0x80 lor ((code lsr 6) land 0x3F)) in
    (* | ________ | ________ | 10xxxxxx | *)
    let b3 = Char.chr (0x80 lor (code land 0x3F)) in
    String.init 3 (function
                   | 0 -> b1
                   | 1 -> b2
                   | 2 -> b3
                   | _ -> assert false)
  in
    QCheck2.Gen.map bytes_of_surrogate_codepoint gen_surrogate_codepoint

let gen_unicode_string_with_surrogate =
  QCheck2.Gen.(int_range 1 50 >>= fun len ->
    let gen_pos = int_range 0 (len - 1) in
    let gen_valid_uchars = list_size (return len) gen_unicode_char in
    map3 (fun pos uchars surrogate_str ->
            let buf = Buffer.create len in
            List.iteri (fun i uchar ->
                          if i = pos then Buffer.add_string buf surrogate_str
                          else Buffer.add_utf_8_uchar buf uchar) uchars;
            Buffer.contents buf
         )
         gen_pos
         gen_valid_uchars
         gen_lone_surrogate_string
  )

  (* Helper functions *)

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

let is_invalid_arg_prefix_invalid_unicode = function
  | Invalid_argument msg -> String.starts_with "Invalid Unicode:" msg
  | _ -> false

(* Properties *)

let () =
  let that ?(count=200) name =
    QCheck2.Test.make
      ~name ~count in

  let run_tests tests = QCheck_base_runner.run_tests_main tests in
  [
    that "is_encodable_num: Invalid negative integer range" invalid_neg_fi_int_range ~print:string_of_int
      (fun i ->
        float_of_int i |> Json_JCSnafi.is_encodable_num |> not
      );
    that "is_encodable_num: Invalid positive integer range" invalid_pos_fi_int_range ~print:string_of_int
      (fun i ->
        float_of_int i |> Json_JCSnafi.is_encodable_num |> not
      );

    that "unparse_jcsnafi: Invalid negative integer range" invalid_neg_fi_int_range ~print:string_of_int
      (fun i ->
        is_exn (Invalid_argument "float or out-of-range integer")
          (fun () -> Json_JCSnafi.unparse_jcsnafi(`num (float_of_int i)))
      );
    that "unparse_jcsnafi: Invalid positive integer range" invalid_pos_fi_int_range ~print:string_of_int
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
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`obj [(s, `null)]))); (* TODO JSON.jv generator *)
    that "unparse_jcsnafi: Unicode surrogate codepoint range" gen_unicode_string_with_surrogate ~print:identity
      (fun s ->
        is_exn_p is_invalid_arg_prefix_invalid_unicode
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`str s)));
    that "unparse_jcsnafi: Unicode surrogate codepoint range in object property name" gen_unicode_string_with_surrogate ~print:identity
      (fun s ->
        is_exn_p is_invalid_arg_prefix_invalid_unicode
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`obj [(s, `null)]))); (* TODO JSON.jv generator *)
  ] |> run_tests |> exit
