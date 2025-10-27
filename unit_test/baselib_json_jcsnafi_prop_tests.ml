let min_fi_int = (- (1 lsl 52))
let max_fi_int = (1 lsl 52) - 1

(* Generators *)

let gen_fi_int = QCheck2.Gen.int_range min_fi_int max_fi_int
let gen_fi_float = QCheck2.Gen.map float_of_int gen_fi_int
let invalid_neg_fi_int_range = QCheck2.Gen.int_range min_int (pred min_fi_int)
let invalid_pos_fi_int_range = QCheck2.Gen.int_range (succ max_fi_int) max_int

let gen_random_byte_string =
  QCheck2.Gen.(map String.of_list (list_size (int_range 1 100) char))

let gen_unicode_char =
  QCheck2.Gen.(
    let gen_unicode_codepoint = oneof [int_range 0x0000 0xD7FF; int_range 0xE000 0x10FFFF] in
    map Uchar.of_int gen_unicode_codepoint
  )

let gen_uchar_list len =
  QCheck2.Gen.(
    int_range 1 len >>= fun len' ->
    list_size (pure len') gen_unicode_char
  )

let string_of_uchars uchars =
  let buf = Buffer.create (List.length uchars) in
  List.iter (Buffer.add_utf_8_uchar buf) uchars;
  Buffer.contents buf

let gen_unicode_string_len len = QCheck2.Gen.map string_of_uchars (gen_uchar_list len)

let gen_unicode_string = gen_unicode_string_len 50

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

let string_of_uchars_with_inj pos inj uchars =
  let buf = Buffer.create (List.length uchars  + String.length inj) in
  List.iteri (fun i uchar ->
    if i = pos then Buffer.add_string buf inj
    else Buffer.add_utf_8_uchar buf uchar
  ) uchars;
  Buffer.contents buf

let gen_unicode_string_with_surrogate_len len =
  QCheck2.Gen.(gen_uchar_list len >>= fun uchars ->
    let len' = List.length uchars in
    let gen_pos = int_range 0 (len' - 1) in
    map3 string_of_uchars_with_inj gen_pos gen_lone_surrogate_string (pure uchars)
  )

let gen_unicode_string_with_surrogate = gen_unicode_string_with_surrogate_len 50

let rec sized_jv size = QCheck2.Gen.(
    let gen_null = pure `null in
    let gen_bool = bool >|= (fun x -> `bool x) in
    let gen_int = gen_fi_int >|= (fun x -> `num (float_of_int x)) in (* TODO: int53p convert *)
    let gen_num = gen_int in
    let gen_str = gen_unicode_string >|= (fun s -> `str s) in
    let gen_atom = oneof [ gen_null; gen_bool; gen_num; gen_str; ] in

    let gen_len n = int_range 0 (n / 2) in

    let gen_arr n =
      (if n > 1 then
         gen_len n >>= fun len ->
         if len = 0 then pure []
         else list_size (int_bound len) (sized_jv (n / len))
       else pure [])
      >|= (fun xs -> `arr xs) in

    let gen_unique_fnames len =
      let module StringSet = Set.Make(String) in
      map (fun l -> StringSet.of_list l |> StringSet.elements)
          (list_size (int_bound len) (gen_unicode_string_len 10)) in

    let gen_obj n =
      (if n > 1 then
         gen_len n >>= fun len ->
         gen_unique_fnames len >>= (fun keys ->
           if keys = [] then pure []
           else
              map2 (fun ks vs -> List.combine ks vs)
              (pure keys)
              (list_repeat (List.length keys) (sized_jv (n / List.length keys))))
       else pure [])
       >|= (fun fs -> `obj fs) in

    match size with
    | 0 -> pure `null
    | 1 -> gen_atom
    | _ -> frequency [(1, gen_atom);
                      (4, gen_arr size);
                      (5, gen_obj size);
                     ])

let gen_jv_jcsnafi = QCheck2.Gen.sized sized_jv

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
        does_throw (Invalid_argument "float or out-of-range integer")
          (fun () -> Json_JCSnafi.unparse_jcsnafi(`num (float_of_int i)))
      );
    that "unparse_jcsnafi: Invalid positive integer range" invalid_pos_fi_int_range ~print:string_of_int
      (fun i ->
        does_throw (Invalid_argument "float or out-of-range integer")
          (fun () -> Json_JCSnafi.unparse_jcsnafi(`num (float_of_int i)))
      );
    that "unparse_jcsnafi: Invalid UTF-8 string" gen_random_byte_string ~print:identity
      (fun s ->
        QCheck2.assume (is_invalid_utf8 s);
        does_throw_p is_invalid_arg_prefix_invalid_unicode
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`str s)));
    that "unparse_jcsnafi: Invalid UTF-8 object property name" gen_random_byte_string ~print:identity
      (fun s ->
        QCheck2.assume (is_invalid_utf8 s);
        does_throw_p is_invalid_arg_prefix_invalid_unicode
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`obj [(s, `null)]))); (* TODO JSON.jv generator *)
    that "unparse_jcsnafi: Unicode surrogate codepoint range" gen_unicode_string_with_surrogate ~print:identity
      (fun s ->
        does_throw_p is_invalid_arg_prefix_invalid_unicode
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`str s)));
    that "unparse_jcsnafi: Unicode surrogate codepoint range in object property name" gen_unicode_string_with_surrogate ~print:identity
      (fun s ->
        does_throw_p is_invalid_arg_prefix_invalid_unicode
          (fun () -> Json_JCSnafi.unparse_jcsnafi (`obj [(s, `null)]))); (* TODO JSON.jv generator *)
  ] |> run_tests |> exit
