open Baselib_test_lib.Json

let pp_jvpath_component ppf = function
  | `f str -> fprintf ppf "`f %S" str
  | `i idx -> fprintf ppf "`i %d" idx
let pp_jvpath = List.pp pp_jvpath_component
let show_jvpath = sprintf "%a" pp_jvpath

let () =
  let that ?(count=200) name =
    QCheck2.Test.make
      ~name ~count in
  [
    that "parse/unparse_jvpath" gen_jvpath ~print:show_jvpath
      (fun path ->
        let unparsed = path |> Json.unparse_jvpath in
        try
          let result = unparsed |> Json.parse_jvpath_exn in
          let eq = path = result in
          if not eq then (
          Log0.debug "[FAIL] %a =o=r= %a >> %S"
            pp_jvpath path pp_jvpath result
            unparsed;
          );
          eq
        with e ->
          Log0.debug "[FAIL] %a >> %S" pp_jvpath path unparsed;
          raise e
      );
  ] |> QCheck_base_runner.run_tests_main |> exit
