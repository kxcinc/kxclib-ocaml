open Kxclib_priv_test_lib.Json
open Kxclib.Json

let min_fi_float = -. (2. ** 52.)
let max_fi_float = (2. ** 52.) -. 1.
let min_fi_int53p = Int53p.of_float min_fi_float
let max_fi_int53p = Int53p.of_float max_fi_float

(* Generators *)

let gen_int64_range (low_i64 : int64) (high_i64 : int64) : int64 QCheck2.Gen.t =
  if low_i64 > high_i64 then raise (Invalid_argument "high_i64 < low_i64")
  else
    (* Add 1L to calculate the number of values *)
    let range_width = Int64.add (Int64.sub high_i64 low_i64) 1L in

    (* When `range_with` is 0L,
       [Int64.min_int, Int64.max_int] is specified. *)
    if range_width = 0L then
      QCheck2.Gen.int64
    else
      (* Map the range of Gen.int64 to [low_i64, high_i64] *)
      QCheck2.Gen.map (fun i64 ->
          let rem = Int64.rem i64 range_width in
          
          (* If the remainder is negative, adjust it to 0 or greater. *)
          let offset = if rem < 0L then Int64.add rem range_width else rem in
          
          (* By adding the offset to the minimum value (low_i64),
             values within the range are generated. *)
          Int64.add low_i64 offset
        ) QCheck2.Gen.int64

let gen_fi_int53p : int53p QCheck2.Gen.t = 
  let min_fi_int53p_int64 = Int53p.to_int64 min_fi_int53p in
  let max_fi_int53p_int64 = Int53p.to_int64 max_fi_int53p in
  QCheck2.Gen.map Int53p.of_int64 (gen_int64_range min_fi_int53p_int64 max_fi_int53p_int64)

let invalid_neg_fi_int64 = gen_int64_range Int64.min_int (Int64.pred (Int53p.to_int64 min_fi_int53p))
let invalid_pos_fi_int64 = gen_int64_range (Int64.succ (Int53p.to_int64 max_fi_int53p)) Int64.max_int

let gen_invalid_utf8_char = QCheck2.Gen.(
  oneof [
    char_range (char_of_int 128) (char_of_int 191);
    char_range (char_of_int 192) (char_of_int 193);
    char_range (char_of_int 245) (char_of_int 255);
  ])

let gen_random_invalid_utf8_string = QCheck2.Gen.(
  map String.of_list (list_size (int_range 1 100) gen_invalid_utf8_char)
)

let gen_unicode_char =
  QCheck2.Gen.(
    let gen_unicode_codepoint = oneof [int_range 0x0000 0xD7FF; int_range 0xE000 0x10FFFF] in
    map Uchar.of_int gen_unicode_codepoint
  )

let gen_uchar_list len =
  QCheck2.Gen.(
    int_range 0 len >>= fun len' ->
    list_size (pure len') gen_unicode_char
  )

let string_of_uchars uchars =
  let buf = Buffer.create (List.length uchars) in
  List.iter (Buffer.add_utf_8_uchar buf) uchars;
  Buffer.contents buf

let gen_unicode_string_len len = QCheck2.Gen.map string_of_uchars (gen_uchar_list len)

let gen_unicode_string = gen_unicode_string_len 10

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

let gen_unicode_string_with_surrogate_len len = QCheck2.Gen.(
  map3 (fun prefix invalid suffix -> prefix ^ invalid ^ suffix)
       (gen_unicode_string_len len)
       gen_lone_surrogate_string
       (gen_unicode_string_len len))

let gen_unicode_string_with_surrogate = gen_unicode_string_with_surrogate_len 10

let gen_unicode_string_with_invalid_unicode_string_len len = QCheck2.Gen.(
  map3 (fun prefix invalid suffix -> prefix ^ invalid ^ suffix)
       (gen_unicode_string_len len)
       gen_random_invalid_utf8_string
       (gen_unicode_string_len len))

let gen_unicode_string_with_invalid_unicode_string = gen_unicode_string_with_invalid_unicode_string_len 10

let gen_unique_fnames len = QCheck2.Gen.(
  let module StringSet = Set.Make(String) in
  map (fun l -> StringSet.of_list l |> StringSet.elements)
      (list_repeat len gen_unicode_string))

let rec sized_jv size = QCheck2.Gen.(
  let gen_null = pure `null in
  let gen_bool = bool >|= (fun x -> `bool x) in
  let gen_int = gen_fi_int53p >|= (fun x -> `num (Int53p.to_float x)) in
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

  let gen_obj n =
    (if n > 1 then
       gen_len n >>= fun len ->
         gen_unique_fnames len >>= (fun keys ->
           let keys_len = List.length keys in
           if keys_len = 0 then pure []
           else
             map2 (fun ks vs -> List.combine ks vs)
             (pure keys)
             (list_repeat keys_len (sized_jv (n / keys_len))))
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

let rec gen_shuffled_jv jv = QCheck2.Gen.(
  match jv with
  | `obj es ->
      shuffle_l es >>= (fun shuffled_es ->
        let shuffled_all = List.map (fun (k, v) ->
          gen_shuffled_jv v >|= (fun v' -> (k, v'))) shuffled_es in
          flatten_l shuffled_all >|= (fun obj' -> `obj obj'))
  | `arr xs ->
      let shuffled_xs = List.map gen_shuffled_jv xs in
      flatten_l shuffled_xs >|= (fun xs' -> `arr xs')
  | _ -> pure jv)

let gen_jv_and_shuffled_pair = QCheck2.Gen.(
  gen_jv_jcsnafi >>= fun jv ->
  gen_shuffled_jv jv >>= fun shuffled_jv ->
  pure (jv, shuffled_jv))

let gen_jv_with_duplicate_keys = QCheck2.Gen.(sized (fun size ->
    (* n is number of object keys *)
    let n = max 2 size in
    int_range 1 (max 1 (n / 2)) >>= fun len ->

    (* create valid key-val pair *)
    gen_unique_fnames len >>= fun keys ->
    let keys_len = List.length keys in
    let val_size = if keys_len = 0 then 0
                   else (n - 1) / keys_len in
    list_repeat keys_len (sized_jv val_size) >>= fun vals ->
    let valid_pairs = List.combine keys vals in

    (* create duplicated key *)
    oneofl keys >>= fun dup_key ->

    (* create new value for duplicated key *)
    sized_jv (max 1 (n / 2)) >>= fun val' ->

    let invalid_pairs = (dup_key, val') :: valid_pairs in
    pure (`obj invalid_pairs)
  ))

let rec contains_str_or_obj (jv : jv) : bool =
  match jv with
  | `str _ -> true
  | `arr xs -> List.exists contains_str_or_obj xs
  | `obj es ->
      List.length es > 0
      || List.exists (fun (_, v) -> contains_str_or_obj v) es
  | _ -> false

let rec filter_gen p gen = QCheck2.Gen.(
  gen >>= fun v -> if p v then pure v
                   else filter_gen p gen
)

let gen_jv_with_str_or_obj = filter_gen contains_str_or_obj gen_jv_jcsnafi

let rec gen_inject_one_bad_string bad_string_gen jv = QCheck2.Gen.(
  match jv with
  | `str _ ->
      bad_string_gen >|= (fun s -> `str s)

  | `arr xs ->
      let candidates = List.mapi (fun i x -> (i, x)) xs in
      let replaceable_candidates = List.filter (fun ix -> snd ix |> contains_str_or_obj) candidates in

      if replaceable_candidates = [] then pure (`arr xs)
      else
        oneofl replaceable_candidates >>= fun (i', target) ->
        gen_inject_one_bad_string bad_string_gen target >>= fun target' ->
        let xs' = List.mapi (fun i x -> if i = i' then target' else x) xs in
        pure (`arr xs')

  | `obj es ->
      let action_lists =
        List.mapi (fun i (_, v) ->
          
          let action_key () =
            bad_string_gen >>= fun bad_key ->
            let es' = List.mapi (fun i' (k, v) ->
              if i = i' then (bad_key, v) else (k, v)
            ) es in
            pure (`obj es')
          in

          if contains_str_or_obj v then
            let action_val () =
              gen_inject_one_bad_string bad_string_gen v >>= fun v' ->
              let es' = List.mapi (fun i' (k, v) ->
                if i = i' then (k, v') else (k, v)
              ) es in
              pure (`obj es')
            in
            [action_val; action_key]
          else
            [action_key]

        ) es
      in
      
      let actions = List.flatten action_lists in

      if actions = [] then
        pure (`obj es)
      else
        oneofl actions >>= fun selected_action ->
        selected_action ()

  | _ -> pure jv
)

let gen_jv_with_invalid_unicode = QCheck2.Gen.(
  no_shrink gen_jv_with_str_or_obj >>=
    gen_inject_one_bad_string gen_unicode_string_with_invalid_unicode_string)

let gen_jv_with_surrogates = QCheck2.Gen.(
  no_shrink gen_jv_with_str_or_obj >>=
    gen_inject_one_bad_string gen_unicode_string_with_surrogate)

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

let is_invalid_arg_prefix prefix = function
  | Invalid_argument msg -> String.starts_with prefix msg
  | _ -> false

let is_whitespace = function
 | ' ' | '\t' | '\n' | '\r'  -> true
 | _ -> false

let count_preceding_backslashes s i =
  let rec count j cnt = 
    if j < 0 || s.[j] <> '\\' then cnt
    else count (j - 1) (cnt + 1)
in
  count (i - 1) 0

let has_extra_whitespace_outside_string s =
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
  ] |> run_tests |> exit
