open Js_of_ocaml
open Baselib_test_lib.Json

type jv = Kxclib.Json.jv

let cast = Obj.magic
let json_parse s =
  Js.Unsafe.(
    meth_call
      (pure_js_expr "JSON")
      "parse" [| s |> cast |])
  |> cast

let display_value ?msg x =
  Js.Unsafe.(
    meth_call
      (pure_js_expr "console")
    "log" (match msg with
           | None -> [| cast x |]
           | Some msg ->
              [| Js.string msg |> cast; cast x |]
  )) |> ignore
[@@warning "-32"]

let display_json ?msg x =
  let template = match msg with
    | None -> "%j"
    | Some msg -> msg^": %j" in
  Js.Unsafe.(
    meth_call
      (pure_js_expr "console")
    "log"
    [| Js.string template |> cast; cast x |])
  |> ignore

let test_xjv_round_trip : jv -> bool = fun x ->
  let open Json_ext in
  let x' = x |> (to_xjv &> of_xjv) in
  Kxclib.Json.eqv x x'
  |-> (function
       | true ->
          display_json ~msg:"[OK] round-trip: "
            (x |> to_xjv)
       | false ->
          display_json ~msg:"[FAIL] round-trip: "
            (`obj ["expected", x;
                   "actual", x'; ]
             |> to_xjv))

let () =
  let open Json_ext in
  let successful = ref true in
  let go x =
    if not (test_xjv_round_trip x)
    then successful := false
  in
  let go_raw ~print expected actual =
    if actual = expected then (
      printf "[OK] raw: %s@." (print expected)
    ) else (
      successful := false;
      printf "[FAIL] raw: { expected = %s ; actual = %s}@."
        (print expected) (print actual)
    )
  in
  go_raw ~print:string_of_jv
    (`str "日本語")
    (json_parse (Js.string {|"日本語"|}) |> cast |> of_xjv);
  go_raw ~print:identity
    (to_json_string (`arr [`bool true; `str "hello?"]))
    {|[true,"hello?"]|};
  go_raw ~print:(sprintf "%a" Option.(pp pp_jv))
    (of_json_string_opt {|[true,"hello?"]|})
    (`arr [`bool true; `str "hello?"] |> some);
  go (`null);
  go (`bool true);
  go (`bool false);
  go (`num 131.);
  go (`num 131.338);
  go (`str "hello?");
  go (`str "\127a");
  (* go (`str "\128a"); (* todo.future *) *)
  (* go (`str "\200"); (* todo.future *) *)
  go (`str "日本語");
  go (`str (Js.to_string (Json.unsafe_input (Js.string {|"\u65E5\u672C\u8A9E"|}))));
  go (`arr []);
  go (`obj ["", `obj []]);
  go (`arr [`arr []]);
  go (`arr [`num 1.; `str "(a number)"]);
  go (`obj ["best", `arr []; "friend", `num 23.]);
  printf "done manual tests in %s: %s@." __FILE__
    (if !successful then "successful" else "failed");
  if not !successful then exit 1

let () =
  let gen_jv = gen_jv' ~has_non_printable_string:false in
  let that ?(count=200) name =
    QCheck2.Test.make
      ~name ~count in
  [
    that "Js.to/of_string"
      QCheck2.Gen.string_printable
      (* todo.future - find an efficient solution for non-printable strings *)
      ~print:(
        fun s -> sprintf "\"%s\" (len=%d)"
                   (String.escaped s) (String.length s))
      (fun s ->
        let open Js_of_ocaml in
        s = (Js.string &> Js.to_string) s
      );
    that "Json.eqv" gen_jv ~print:string_of_jv
      (fun j -> Kxclib.Json.eqv j j);
    that "to/of_xjv" gen_jv ~print:string_of_jv
      test_xjv_round_trip;
  ] |> QCheck_base_runner.run_tests |> exit
