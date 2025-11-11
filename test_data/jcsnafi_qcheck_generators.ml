open Kxclib.Json

let min_fi_float : float = -. (2.0 ** 52.0)
let max_fi_float : float = (2.0 ** 52.0) -. 1.0
let min_fi : int53p = Int53p.of_float min_fi_float
let max_fi : int53p = Int53p.of_float max_fi_float

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

let gen_int64 : int64 QCheck2.Gen.t =
  gen_int64_range Int64.min_int Int64.max_int

let gen_fi : int53p QCheck2.Gen.t = 
  let min_fi_int53p_int64 = Int53p.to_int64 min_fi in
  let max_fi_int53p_int64 = Int53p.to_int64 max_fi in
  QCheck2.Gen.map Int53p.of_int64 (gen_int64_range min_fi_int53p_int64 max_fi_int53p_int64)

let invalid_neg_fi_int64 = gen_int64_range Int64.min_int (Int64.pred (Int53p.to_int64 min_fi))
let invalid_pos_fi_int64 = gen_int64_range (Int64.succ (Int53p.to_int64 max_fi)) Int64.max_int

let gen_invalid_utf8_char : char QCheck2.Gen.t = QCheck2.Gen.(
  oneof [
    char_range (char_of_int 128) (char_of_int 191);
    char_range (char_of_int 192) (char_of_int 193);
    char_range (char_of_int 245) (char_of_int 255);
  ])

let gen_random_invalid_utf8_string : string QCheck2.Gen.t = QCheck2.Gen.(
  map String.of_list (list_size (int_range 1 100) gen_invalid_utf8_char)
)

let gen_unicode_char : Uchar.t QCheck2.Gen.t = QCheck2.Gen.(
    let gen_unicode_codepoint = oneof [int_range 0x0000 0xD7FF; int_range 0xE000 0x10FFFF] in
    map Uchar.of_int gen_unicode_codepoint)

let gen_uchar_list (len : int) : Uchar.t list QCheck2.Gen.t = QCheck2.Gen.(
    int_range 0 len >>= fun len' ->
    list_size (pure len') gen_unicode_char)

let string_of_uchars (uchars : Uchar.t list) : string =
  let buf = Buffer.create (List.length uchars) in
  List.iter (Buffer.add_utf_8_uchar buf) uchars;
  Buffer.contents buf

let gen_unicode_string_len (len : int) : string QCheck2.Gen.t =
  QCheck2.Gen.map string_of_uchars (gen_uchar_list len)

let gen_unicode_string : string QCheck2.Gen.t =
  gen_unicode_string_len 10

let gen_lone_surrogate_string : string QCheck2.Gen.t =
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

let gen_unicode_string_with_surrogate_len (len : int) : string QCheck2.Gen.t = QCheck2.Gen.(
  map3 (fun prefix invalid suffix -> prefix ^ invalid ^ suffix)
       (gen_unicode_string_len len)
       gen_lone_surrogate_string
       (gen_unicode_string_len len))

let gen_unicode_string_with_surrogate : string QCheck2.Gen.t =
  gen_unicode_string_with_surrogate_len 10

let gen_unicode_string_with_invalid_unicode_string_len (len : int) : string QCheck2.Gen.t = QCheck2.Gen.(
  map3 (fun prefix invalid suffix -> prefix ^ invalid ^ suffix)
       (gen_unicode_string_len len)
       gen_random_invalid_utf8_string
       (gen_unicode_string_len len))

let gen_unicode_string_with_invalid_unicode_string : string QCheck2.Gen.t =
  gen_unicode_string_with_invalid_unicode_string_len 10

let gen_valid_or_invalid_unicode_string : string QCheck2.Gen.t = QCheck2.Gen.(
  oneof [
    gen_unicode_string;
    gen_unicode_string_with_invalid_unicode_string;
    gen_unicode_string_with_surrogate
  ])

let gen_unique_fnames (len : int) : string list QCheck2.Gen.t = QCheck2.Gen.(
  let module StringSet = Set.Make(String) in
  map (fun l -> StringSet.of_list l |> StringSet.elements)
      (list_repeat len gen_unicode_string))

let rec sized_jv (size : int) : jv QCheck2.Gen.t = QCheck2.Gen.(
  let gen_null = pure `null in
  let gen_bool = bool >|= (fun x -> `bool x) in
  let gen_num = gen_fi >|= (fun x -> `num (Int53p.to_float x)) in
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

let gen_jv_jcsnafi : jv QCheck2.Gen.t =
  QCheck2.Gen.sized sized_jv

let rec gen_shuffled_jv (jv : jv) : jv QCheck2.Gen.t = QCheck2.Gen.(
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

let gen_jv_and_shuffled_pair : (jv * jv) QCheck2.Gen.t = QCheck2.Gen.(
  gen_jv_jcsnafi >>= fun jv ->
  gen_shuffled_jv jv >>= fun shuffled_jv ->
  pure (jv, shuffled_jv))

let gen_jv_with_duplicate_keys : jv QCheck2.Gen.t = QCheck2.Gen.(sized (fun size ->
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

let rec filter_gen (p : 'a -> bool) (gen : 'a QCheck2.Gen.t) : 'a QCheck2.Gen.t = QCheck2.Gen.(
  gen >>= fun v -> if p v then pure v
                   else filter_gen p gen)

let gen_jv_with_str_or_obj : jv QCheck2.Gen.t =
  filter_gen contains_str_or_obj gen_jv_jcsnafi

let rec gen_inject_one_bad_string (bad_string_gen : string QCheck2.Gen.t) (jv : jv) : jv QCheck2.Gen.t = QCheck2.Gen.(
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
      let replaced_es_lists =
        List.mapi (fun i (_, v) ->
          
          let replace_key () =
            bad_string_gen >>= fun bad_key ->
            let es' = List.mapi (fun i' (k, v) ->
              if i = i' then (bad_key, v) else (k, v)
            ) es in
            pure (`obj es')
          in

          if contains_str_or_obj v then
            let replace_val () =
              gen_inject_one_bad_string bad_string_gen v >>= fun v' ->
              let es' = List.mapi (fun i' (k, v) ->
                if i = i' then (k, v') else (k, v)
              ) es in
              pure (`obj es')
            in
            [replace_val; replace_key]
          else
            [replace_key]

        ) es
      in
      
      let replaced_es_list = List.flatten replaced_es_lists in

      if replaced_es_list = [] then
        pure (`obj es)
      else
        oneofl replaced_es_list >>= fun selected_replacement ->
        selected_replacement ()

  | _ -> pure jv
)

let gen_jv_with_invalid_unicode : jv QCheck2.Gen.t = QCheck2.Gen.(
  no_shrink gen_jv_with_str_or_obj >>=
    gen_inject_one_bad_string gen_unicode_string_with_invalid_unicode_string)

let gen_jv_with_surrogates : jv QCheck2.Gen.t = QCheck2.Gen.(
  no_shrink gen_jv_with_str_or_obj >>=
    gen_inject_one_bad_string gen_unicode_string_with_surrogate)

let gen_valid_or_invalid_jv : jv QCheck2.Gen.t = QCheck2.Gen.(
  oneof [
    gen_jv_jcsnafi;
    gen_jv_with_invalid_unicode;
    gen_jv_with_surrogates
  ])
