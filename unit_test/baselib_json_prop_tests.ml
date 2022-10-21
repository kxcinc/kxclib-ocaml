open Json

let rec pp_jv ppf : jv -> unit = function
  | `null -> fprintf ppf "`null"
  | `bool b -> fprintf ppf "`bool %B" b
  | `num f -> fprintf ppf "`num %F" f
  | `str s -> fprintf ppf "`str %S" s
  | `arr xs -> fprintf ppf "`arr %a" (List.pp pp_jv) xs
  | `obj xs -> fprintf ppf "`obj %a" (List.pp (fun ppf (k, v) -> fprintf ppf "(%S, %a)" k pp_jv v)) xs

let gen_jv =
  let module G = QCheck2.Gen in
  let f self size =
    let (>>=) = G.bind in
    let (>|=) = G.(>|=) in
    let gen_null = G.pure `null in
    let gen_bool = G.bool >|= (fun x -> `bool x) in
    let gen_int = G.int >|= (fun x -> `num (float_of_int x)) in
    let gen_float = G.float >|= (fun x -> `num x) in
    let gen_num =
      G.frequency [
          5, gen_int;
          2, gen_float;
        ] in
    let gen_str =
      G.frequency [
          5, G.small_string ~gen:G.printable;
          1, G.small_string ~gen:G.char;
          1, G.string_printable;
          1, G.string;
        ]
      >|= (fun x -> `str x) in
    let gen_atom = G.oneof [ gen_null; gen_bool; gen_num; gen_str; ] in
    let gen_len =
      sqrt (float_of_int size) |> int_of_float
      |> function
        | 0 | 1 -> G.pure 1
        | n -> G.int_range 1 n in
    let gen_arr =
      (if size > 1 then
         gen_len >>= fun len ->
         G.list_size (G.int_bound len) (self (size/len))
       else G.pure [])
      >|= (fun xs -> `arr xs) in
    let gen_fname =
      G.frequency [
          10, G.small_string ~gen:G.printable;
          2, G.small_string ~gen:G.char;
          2, G.string_printable;
          1, G.string;
        ] in
    let gen_field size : (string*jv) G.t =
      gen_fname >>= fun fname ->
      self size >>= fun fval ->
      G.pure (fname, fval) in
    let gen_obj = 
      (if size > 1 then
         gen_len >>= fun len ->
         G.list_size (G.int_bound len) (gen_field (size/len))
       else G.pure [])
      >|= (fun fs -> `obj fs) in
    match size with
    | 0 -> G.pure `null
    | 1 -> gen_atom
    | _ -> (
      G.frequency [
          1, gen_atom;
          2, gen_arr;
          3, gen_obj;
        ]
    )
  in G.(sized @@ fix f)

let () =
  QCheck2.Gen.generate ~n:10 gen_jv
  |!> (info "%a@." pp_jv)

