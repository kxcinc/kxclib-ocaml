open Baselib_test_lib.Json

type jv = Kxclib.Json.jv

let display_json ?msg x =
  let open Js_of_ocaml in
  let cast = Obj.magic in
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
  let successful = ref true in
  let go x =
    if not (test_xjv_round_trip x)
    then successful := false
  in
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
      (fun j -> Json.eqv j j);
    that "to/of_xjv" gen_jv ~print:string_of_jv
      test_xjv_round_trip;
  ] |> QCheck_base_runner.run_tests |> exit
