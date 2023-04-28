open Kxclib_priv_test_lib.Json

let () =
  let that ?(count=200) name =
    QCheck2.Test.make
      ~name ~count in
  [
    that "to/of_yojson" gen_jv ~print:string_of_jv
      (fun j ->
        j = (j |> Json.to_yojson |> Json.of_yojson)
      );
    that "to/of_jsonm" gen_jv ~print:string_of_jv
      (fun j ->
        match j |> Json.to_jsonm |> Json.of_jsonm with
        | Some (j', rest) -> (rest() = Seq.Nil) && j' = j
        | _ -> false);
  ] |> QCheck_base_runner.run_tests_main |> exit

