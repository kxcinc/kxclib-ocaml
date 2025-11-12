open Alcotest
open Kxclib_js
open Jsoo_test_helpers

module Json_equality = struct
  external deep_equal : string -> string -> bool = "json_string_deep_equal"
end

let test_compare_with_yojson =
  let open QCheck2 in
  [
    Test.make ~count:2000 ~name:"comparison between Json_jcsnafi and yojson"
      Jcsnafi_qcheck_generators.gen_jv_jcsnafi
      ~print:(fun jv ->
        let yojson_string = Json.to_yojson jv |> Yojson.Safe.to_string in

        let unparsed = Json_JCSnafi.unparse_jcsnafi jv in
        sprintf
          "input jv:\n\t%s\n\n"
          (show_jv jv)
        ^ sprintf "Yojson.Safe.to_string:\n\t%s\n\t%s\n\n"
          yojson_string
          (yojson_string |> esc)
        ^ sprintf "Json_JCSnafi.unparse_jcsnafi:\n\t%s\n\t%s\n\n"
          unparsed
          (unparsed |> esc)
        ^ sprintf "re-interp:\n\nYojson.Safe.to_string:\n\t%s\n\nJson_JCSnafi.unparse_jcsnafi:\n\t%s\n\n"
          (Json_ext.of_json_string_opt yojson_string >? show_jv |? "(invalid-json)")
          (Json_ext.of_json_string_opt unparsed >? show_jv |? "(invalid-json)")
        )
      (fun jv ->
        let yojson_string = Json.to_yojson jv |> Yojson.Safe.to_string in
        let jcsnafi_unparsed = Json_JCSnafi.unparse_jcsnafi jv in
        Json_equality.deep_equal yojson_string jcsnafi_unparsed)
  ] |&> QCheck_alcotest.to_alcotest

let () =
  Printexc.record_backtrace true;
  run "Baselib_json_jcsnafi_yojson_test" ([
    "check external", [
      test_case "deep_equal" `Quick (fun () ->
        [
          `identity {|null|};
          `identity {|1|};
          `identity {|"abcd"|};
          `identity {|[ 1, 2, "a", {"b": "c"}, [ { "d": [ 1, 2 ]} ] ]|};
          `valid ({|{ "a": 1, "b": 2 }|}, {|{ "b": 2, "a": 1 }|});
          `invalid ({|null|}, {|"foobar"|});
          `invalid ({|1|}, {|2|});
          `invalid ({|1|}, {|"1"|});
          `invalid ({|[]|}, {|[0]|});
          `invalid ({|[1, 2, 3]|}, {|[0, 1, 2]|});
        ]
        |> List.map (function
        | `identity s -> (s, s, true)
        | `valid (s1, s2) -> (s1, s2, true)
        | `invalid (s1, s2) -> (s1, s2, false)
        )
        |> List.iter (fun (s1, s2, expected) ->
          check' bool
          ~msg:"It will be success to call external function 'deep_equal'"
          ~expected
          ~actual:(Json_equality.deep_equal s1 s2))
        
        );
    ];
    "compare_with_yojson", test_compare_with_yojson
  ])
