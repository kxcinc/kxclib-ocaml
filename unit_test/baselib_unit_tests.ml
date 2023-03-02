open Alcotest


let sprintf fmt = Format.asprintf fmt
let float_testable = float 0.0001


let trivial () =
  check string "I'm trivial" "hello" "hello"


let seq_iteri n msgs () =
  let open Seq in
  let s = iota n in
  let an_array = Array.make n "" in
  let mk_msg i x = sprintf "Hello %d.%d!" i x in
  let f i x =
    an_array.(i) <- mk_msg i x in
  let expected = msgs in
  let actual = iteri f s; an_array in
  check (array string) "seq_iteri" expected actual


let seq_iteri_0 =
  seq_iteri 3 [|"Hello 0.0!"; "Hello 1.1!"; "Hello 2.2!"|]


let seq_range start end_exclusive expected () =
  let actual = Seq.range start end_exclusive |> Array.of_seq in
  check (array int) "seq_range" expected actual


let seq_range_0 = seq_range 3 7 [|3; 4; 5; 6|]


let seq_make tstbl n x expected () =
  let actual = Seq.make n x |> Array.of_seq in
  check tstbl "seq_make" expected actual


let seq_make_0 = seq_make (array int) 5 2 [|2; 2; 2; 2; 2|]
let seq_make_1 = seq_make (array string) 1 "Hi" [|"Hi"|]
let seq_make_2 = seq_make (array int) 0 2 [||]
let seq_make_3 = seq_make (array float_testable) 0 3.14 ([||])


let seq_take tstbl n org_lst expected_lst () =
  let actual = List.to_seq org_lst |> Seq.take n in
  let actual_lst = List.of_seq actual in
  check tstbl "seq_take" expected_lst actual_lst


let seq_take_0 = seq_take (list int) 2 [2; 3; 4] [2; 3]
let seq_take_1 = seq_take (list string) 0 ["A"; "B"; "C"] []


let seq_drop tstbl n org_lst expected_lst () =
  let actual = List.to_seq org_lst |> Seq.drop n in
  let actual_lst = List.of_seq actual in
  check tstbl "seq_drop" expected_lst actual_lst


let seq_drop_0 = seq_drop (list int) 2 [2; 3; 4] [4]
let seq_drop_1 = seq_drop (list string) 0 ["A"; "B"; "C"] ["A"; "B"; "C"]
let seq_drop_2 = seq_drop (list string) 3 ["A"; "B"; "C"] []


[%%if ocaml_version < (4, 14, 0)]
let stream_take tstbl n org_lst expected_lst () =
  let actual = Stream.of_list org_lst |> Stream.take n in
  let actual_lst = actual in
  check tstbl "stream_take" actual_lst expected_lst


let stream_take_0 = stream_take (list int) 2 [2; 3; 4] [2; 3]
let stream_take_1 = stream_take (list string) 0 ["A"; "B"; "C"] []


let stream_drop tstbl n org_lst expected_lst () =
  let actual = Stream.of_list org_lst |> Stream.drop n in
  let actual_lst = Stream.to_list actual in
  check tstbl "stream_drop" actual_lst expected_lst


let stream_drop_0 = stream_drop (list int) 2 [2; 3; 4] [4]
let stream_drop_1 = stream_drop (list string) 0 ["A"; "B"; "C"] ["A"; "B"; "C"]
let stream_drop_2 = stream_drop (list string) 3 ["A"; "B"; "C"] []
[%%endif]

let base64_known () =
  let rfc4648 = (module Base64 : Base64.T) in
  let rfc4648_url = (module Base64.Url : Base64.T) in
  let cases = [
    (* from RFC4648 *)
    "", "", rfc4648;
    "f", "Zg==", rfc4648;
    "fo", "Zm8=", rfc4648;
    "foo", "Zm9v", rfc4648;
    "foob", "Zm9vYg==", rfc4648;
    "fooba", "Zm9vYmE=", rfc4648;
    "foobar", "Zm9vYmFy", rfc4648;

    (* long strings *)
    "hello, world", "aGVsbG8sIHdvcmxk", rfc4648;
    "hello, world?!", "aGVsbG8sIHdvcmxkPyE=", rfc4648;
    "hello, world.", "aGVsbG8sIHdvcmxkLg==", rfc4648;

    (* very long strings *)
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna.",
    "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwgc2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWduYS4=",
    rfc4648;
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
    "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwgc2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWduYSBhbGlxdWEuIFV0IGVuaW0gYWQgbWluaW0gdmVuaWFtLCBxdWlzIG5vc3RydWQgZXhlcmNpdGF0aW9uIHVsbGFtY28gbGFib3JpcyBuaXNpIHV0IGFsaXF1aXAgZXggZWEgY29tbW9kbyBjb25zZXF1YXQu",
    rfc4648;
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
    "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwgc2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWduYSBhbGlxdWEuIFV0IGVuaW0gYWQgbWluaW0gdmVuaWFtLCBxdWlzIG5vc3RydWQgZXhlcmNpdGF0aW9uIHVsbGFtY28gbGFib3JpcyBuaXNpIHV0IGFsaXF1aXAgZXggZWEgY29tbW9kbyBjb25zZXF1YXQuIER1aXMgYXV0ZSBpcnVyZSBkb2xvciBpbiByZXByZWhlbmRlcml0IGluIHZvbHVwdGF0ZSB2ZWxpdCBlc3NlIGNpbGx1bSBkb2xvcmUgZXUgZnVnaWF0IG51bGxhIHBhcmlhdHVyLiBFeGNlcHRldXIgc2ludCBvY2NhZWNhdCBjdXBpZGF0YXQgbm9uIHByb2lkZW50LCBzdW50IGluIGN1bHBhIHF1aSBvZmZpY2lhIGRlc2VydW50IG1vbGxpdCBhbmltIGlkIGVzdCBsYWJvcnVtLg==",
    rfc4648;

    (* bytes *)
    "\xff", "/w==", rfc4648;
    "\xff\xee", "/+4=", rfc4648;
    "\xff\xee\xdd", "/+7d", rfc4648;
    "\xff\xee\xdd\xcc", "/+7dzA==", rfc4648;
    "\xff\xee\xdd\xcc\xbb", "/+7dzLs=", rfc4648;
    "\xff\xee\xdd\xcc\xbb\xaa", "/+7dzLuq", rfc4648;
    "\xff\xee\xdd\xcc\xbb\xaa\x99", "/+7dzLuqmQ==", rfc4648;
    "\xff\xee\xdd\xcc\xbb\xaa\x99\x88", "/+7dzLuqmYg=", rfc4648;

    (* edge cases *)
    "\x00", "AA==", rfc4648;
    "\x00\x00", "AAA=", rfc4648;
    "\x00\x00\x00", "AAAA", rfc4648;
    "\xff", "/w==", rfc4648;
    "\xff\xff", "//8=", rfc4648;
    "\xff\xff\xff", "////", rfc4648;

    (* base64url *)
    "\xff\xee\xdd\xcc", "_-7dzA", rfc4648_url;
  ] in
  cases |> List.iter (fun (plain, code, (module B64: Base64.T)) ->
    let expected_plain = Bytes.of_string plain in
    let expected_code = code in
    let actual_code = B64.encode expected_plain in
    check string (sprintf "base64: encode '%s' => %s" plain expected_code) expected_code actual_code;
    let actual_plain = B64.decode expected_code in
    check bytes (sprintf "base64: decode '%s' => %s" code (String.escaped plain)) expected_plain actual_plain
  )

let base64_range () =
  let orig = Bytes.of_string "\xff\xee\xdd\xcc\xbb\xaa\x99\x88" in
  let input_cases = [
    (* offset, len, expected_code *)
    0, 8, "/+7dzLuqmYg=";
    0, 7, "/+7dzLuqmQ==";
    0, 6, "/+7dzLuq";
    1, 7, "7t3Mu6qZiA==";
    2, 6, "3cy7qpmI";
    3, 5, "zLuqmYg=";
    1, 6, "7t3Mu6qZ";
    2, 4, "3cy7qg==";
    3, 2, "zLs=";
    4, 0, "";
  ] in
  input_cases |> List.iter (fun (offset, len, expected_code) ->
    let actual_code = Base64.encode ~offset ~len orig in
    check string
      (sprintf "base64: encode with range (offset=%d, len=%d) => '%s'" offset len expected_code)
      expected_code actual_code
  );
  let output_cases = [
    (* code, offset, len, expected_plain *)
    "data:text/plain;base64,SGVsbG8sIFdvcmxkIQ==", 23, None, "Hello, World!";
    "{'data': 'SGVsbG8sIFdvcmxkIQ=='}", 10, Some 20, "Hello, World!";
  ] in
  output_cases |> List.iter (fun (code, offset, len, expected_plain) ->
    let actual_plain = Base64.decode ~offset ?len code in
    check bytes
      (sprintf "base64: decode '%s' with range (offset=%d, len=%s) => '%s'"
        code offset (match len with None -> "None" | Some d -> sprintf "%d" d) expected_plain)
      (Bytes.of_string expected_plain) actual_plain
  )

let base64_decode_pad () =
  let input_cases = [
    "Zm8", none;
    "Zm8=", some "fo";
    "Zm8==", none;
    "Zm8===", none;
    "Zm8=====", none;
    "Zm9v", some "foo";
    "Zm9v====", none;
    "Zg", none;
    "Zg=", none;
    "Zg==", some "f";
    "Zg===", none;
    "Zg==\n", some "f";
    "Zg\n==\n", some "f";
    "Z\ng\n=\n=\n", some "f";
    "Zm9vYmE=", some "fooba";
    "Zm9v\nYmE=", some "fooba";
    "Zm9v\nYmE=\n", some "fooba";
    "Zm9v\nYmE", none;
    "Zm9v\nYmE\n", none;
  ] in
  input_cases |> List.iter (fun (input_case, expected) ->
    match expected with
    | Some v ->
      let actual_plain_bytes = Base64.decode input_case in
      let actual_plain_string = Bytes.to_string actual_plain_bytes in
      check string input_case v actual_plain_string
    | None ->
      try
        let _ = Base64.decode input_case in
        fail "Invalid_argument expected"
      with
      | Invalid_argument e ->
        Log0.verbose "%s" e;
        ()
      | e -> raise e
  )


type jv = Json.jv


let rec pp_jv ppf : jv -> unit = function
  | `null -> fprintf ppf "`null"
  | `bool b -> fprintf ppf "`bool %B" b
  | `num f -> fprintf ppf "`num %F" f
  | `str s -> fprintf ppf "`str %S" s
  | `arr xs -> fprintf ppf "`arr %a" (List.pp pp_jv) xs
  | `obj xs -> fprintf ppf "`obj %a" (List.pp (fun ppf (k, v) -> fprintf ppf "(%S, %a)" k pp_jv v)) xs

let rec equal_jv (jv1 : jv) (jv2 : jv) = match jv1, jv2 with
  | `null, `null -> true
  | `bool b1, `bool b2 -> b1 = b2
  | `num f1, `num f2 -> f1 = f2
  | `str s1, `str s2 -> s1 = s2
  | `arr a1, `arr a2->
    List.for_all2 equal_jv
      (List.sort compare a1)
      (List.sort compare a2)
  | `obj o1, `obj o2 ->
    List.for_all2
      (fun (k1, v1) (k2, v2) -> k1 = k2 && (equal_jv v1 v2))
      (List.sort compare o1)
      (List.sort compare o2)
  | _, _ -> false

let jv = testable pp_jv equal_jv

let json_of_jsonm jsonm_token_list expected () =
  let actual = jsonm_token_list |> List.to_seq |> Json.of_jsonm >? fst in
  check (option jv) "Json.of_jsonm" (some expected) actual

let json_of_jsonm_null = json_of_jsonm [`Null] `null
let json_of_jsonm_bool_0 = json_of_jsonm [`Bool true] (`bool true)
let json_of_jsonm_bool_1 = json_of_jsonm [`Bool false] (`bool false)
let json_of_jsonm_num_0 = json_of_jsonm [`Float 0.] (`num 0.)
let json_of_jsonm_num_1 = json_of_jsonm [`Float 42.] (`num 42.)
let json_of_jsonm_num_2 = json_of_jsonm [`Float (-12.4)] (`num (-12.4))
let json_of_jsonm_str_0 = json_of_jsonm [`String ""] (`str "")
let json_of_jsonm_str_1 = json_of_jsonm [`String "hello"] (`str "hello")
let json_of_jsonm_str_2 = json_of_jsonm [`String "こんにちは"] (`str "こんにちは")
let json_of_jsonm_arr_0 =
  json_of_jsonm
    [`As; `Ae]
    (`arr [])
let json_of_jsonm_arr_1 =
  json_of_jsonm
    [`As; `Null; `Ae]
    (`arr [`null])
let json_of_jsonm_arr_2 =
  json_of_jsonm
    [`As; `Null; `Bool true; `Ae]
    (`arr [`null; `bool true])
let json_of_jsonm_arr_3 =
  json_of_jsonm
    [`As; `Null; `Bool false; `Float 54.2; `Ae]
    (`arr [`null; `bool false; `num 54.2])
let json_of_jsonm_arr_4 =
  json_of_jsonm
    [`As; `Null; `Bool true; `Float 9.; `String "hello"; `Ae]
    (`arr [`null; `bool true; `num 9.; `str "hello"])
let json_of_jsonm_arr_5 =
  json_of_jsonm
    [`As; `As; `Ae; `As; `Float 0.; `Ae; `As; `As; `Float 1.; `Float 2.; `Ae; `As; `Float 3.; `Ae; `Ae; `Float 4.; `Ae]
    (`arr [`arr []; `arr [`num 0.]; `arr [`arr [`num 1.; `num 2.]; `arr [`num 3.]]; `num 4.])
let json_of_jsonm_arr_6 =
  json_of_jsonm
    [`As; `Os; `Oe; `Os; `Name "a"; `Null; `Oe; `Os; `Name "b"; `Bool true; `Name "c"; `Float 0.; `Oe;
     `Os; `Name "d"; `String ""; `Name "e"; `String "hello"; `Name "f"; `As; `String "g"; `String "h"; `Ae; `Oe; `Ae]
    (`arr [ `obj []; `obj ["a", `null]; `obj ["b", `bool true; "c", `num 0.];
            `obj ["d", `str ""; "e", `str "hello"; "f", `arr [`str "g"; `str "h"]]; ])
let json_of_jsonm_obj_0 =
  json_of_jsonm
    [`Os; `Oe]
    (`obj [])
let json_of_jsonm_obj_1 =
  json_of_jsonm
    [`Os; `Name "a"; `Null; `Oe]
    (`obj ["a", `null])
let json_of_jsonm_obj_2 =
  json_of_jsonm
    [`Os; `Name "a"; `Null; `Name "b"; `Bool false; `Oe]
    (`obj ["a", `null; "b", `bool false])
let json_of_jsonm_obj_3 =
  json_of_jsonm
    [`Os; `Name "a"; `Null; `Name "b"; `Bool false; `Name "c"; `Float 0.; `Oe]
    (`obj ["a", `null; "b", `bool false; "c", `num 0.])
let json_of_jsonm_obj_4 =
  json_of_jsonm
    [`Os; `Name "a"; `Null; `Name "b"; `Bool false; `Name "c"; `Float 0.; `Name "d"; `String "hello"; `Oe]
    (`obj ["a", `null; "b", `bool false; "c", `num 0.; "d", `str "hello"])
let json_of_jsonm_obj_5 =
  json_of_jsonm
    [`Os; `Name "a"; `Null; `Name "b"; `Bool false; `Name "c"; `Float 0.; `Name "d"; `String "hello";
     `Name "e"; `As; `Ae; `Name "f"; `As; `Float 0.; `Ae; `Name "g"; `As; `Float 0.; `Float 1.; `Ae;
     `Name "h"; `Os; `Oe; `Name "i"; `Os; `Name "a2"; `Null; `Oe; `Name "j"; `Os; `Name "a2"; `Null; `Name "b2"; `Bool true; `Oe; `Oe]
    (`obj ["a", `null; "b", `bool false; "c", `num 0.; "d", `str "hello";
           "e", `arr []; "f", `arr [`num 0.]; "g", `arr [`num 0.; `num 1.];
           "h", `obj []; "i", `obj ["a2", `null]; "j", `obj ["a2", `null; "b2", `bool true]])

let jvpath_access' (f, msg, typ) path x expected () =
  let actual = f path x in
  check (option typ) msg expected actual

let jvpath_access =
  jvpath_access' (Jv.access, "Json.access", jv)

let jvpath_access_cases = [
    test_case "Json.access_0_1" `Quick (
        jvpath_access [] (`null) (some `null)
      );
    test_case "Json.access_0_2" `Quick (
        jvpath_access [] (`str "hello") (some (`str "hello"))
      );
    test_case "Json.access_0_3" `Quick (
        jvpath_access [] (`arr [`num 0.; `str "abc"]) (some (`arr [`num 0.; `str "abc"]))
      );
    test_case "Json.access_0_4" `Quick (
        jvpath_access []
          (`obj ["f1", `num 0.; "f2", `str "abc"])
          (some (`obj ["f1", `num 0.; "f2", `str "abc"]))
      );
    test_case "Json.access_1_1" `Quick (
        jvpath_access [`i 0]
          (`obj ["f1", `num 0.; "f2", `str "abc"])
          (none)
      );
    test_case "Json.access_1_2" `Quick (
        jvpath_access [`i 0]
          (`arr [])
          (none)
      );
    test_case "Json.access_1_3" `Quick (
        jvpath_access [`i 1]
          (`arr [`bool true])
          (none)
      );
    test_case "Json.access_2_1" `Quick (
        jvpath_access [`i 0]
          (`arr [`num 0.;`str "abc"])
          (some (`num 0.))
      );
    test_case "Json.access_2_2" `Quick (
        jvpath_access [`i 1]
          (`arr [`num 0.;`str "abc"])
          (some (`str "abc"))
      );
    test_case "Json.access_3_1" `Quick (
        jvpath_access [`f "f1"]
          (`obj ["f1", `num 0.; "f2", `str "abc"])
          (some (`num 0.))
      );
    test_case "Json.access_3_2" `Quick (
        jvpath_access [`f "f2"]
          (`obj ["f1", `num 0.; "f2", `str "abc"])
          (some (`str "abc"))
      );
    test_case "Json.access_3_3" `Quick (
        jvpath_access [`f "f3"]
          (`obj ["f1", `num 0.; "f2", `str "abc"])
          (none)
      );
    test_case "Json.access_4_1" `Quick (
        jvpath_access [`f "f1"; `i 1]
          (`obj ["f1", `arr [`num 0.; `bool false]; "f2", `str "abc"])
          (some (`bool false))
      );
    test_case "Json.access_4_2" `Quick (
        jvpath_access [ `i 1; `f "f1";]
          (`arr [`bool false; (`obj ["f1", `num 0.; "f2", `str "abc"])])
          (some (`num 0.))
      );
    test_case "Json.access_int_1" `Quick (
        jvpath_access' (Jv.access_int, "Jv.access_int", int) [`i 1]
          (`arr [`null; `num 1.])
          (some 1)
      );
    test_case "Json.access_int_2" `Quick (
        jvpath_access' (Jv.access_int, "Jv.access_int", int) [`i 1]
          (`arr [`null; `num 1.1])
          (none)
      );
    test_case "Json.access_arr_1'" `Quick (
        jvpath_access' (
            Jv.access_arr' (Jv.access_str [`f "name"]),
            "Jv.access_arr",
            list string) []
          (`arr [`obj ["name", `str "alice"; "age", `num 12.];
                 `obj ["name", `str "bob"; "age", `num 13.];
                 `obj ["name", `str "cathy"; "age", `num 11.];])
          (some ["alice"; "bob"; "cathy"])
      );
    test_case "Json.access_arr_2'" `Quick (
        jvpath_access' (
            Jv.access_arr' (Jv.access_int [`f "age"]),
            "Jv.access_arr",
            list int) []
          (`arr [`obj ["name", `str "alice"; "age", `num 12.];
                 `obj ["name", `str "bob"; "age", `num 13.];
                 `obj ["name", `str "cathy"; "age", `num 11.];])
          (some [12; 13; 11])
      );
    test_case "Json.access_arr_3'" `Quick (
        jvpath_access' (
            Jv.access_arr' Jv.(fun person ->
              access_int [`f "age"] person >>? fun age ->
              access_str [`f "name"] person >>? fun name ->
              some (name, age)),
            "Jv.access_arr",
            list (pair string int)) []
          (`arr [`obj ["name", `str "alice"; "age", `num 12.];
                 `obj ["name", `str "bob"; "age", `num 13.];
                 `obj ["name", `str "cathy"; "age", `num 11.];])
          (some ["alice", 12;
                 "bob", 13;
                 "cathy", 11])
      );
  ]

[%%if ocaml_version < (4, 14, 0)]
let stream_suite = [
  "stream_take", [
    test_case "stream_take_0" `Quick stream_take_0;
    test_case "stream_take_1" `Quick stream_take_1
  ];
  "stream_drop", [
    test_case "stream_drop_0" `Quick stream_drop_0;
    test_case "stream_drop_1" `Quick stream_drop_1;
    test_case "stream_drop_2" `Quick stream_drop_2
  ];
]
[%%else]
let stream_suite = []
[%%endif]

let json_escaped_suite =
  let counter = ref 0 in
  let case orig escaped =
    let id = get_and_incr counter in
    test_case (sprintf "json_escaped_%d" id) `Quick (fun () ->
        check string
          (sprintf "json_escape: %s => %s"
             (String.escaped orig) escaped)
          escaped (String.json_escaped orig)
      )
  in [
    "json_escaped", [
      case "" {||};
      case "abc" {|abc|};
      case "a\nbc" {|a\nbc|};
      case "a\127bc" {|a\u007fbc|};
      case "\196\163" {|ģ|};
      case "\230\151\165" {|日|};
      case "\xF0\x93\x83\x93" {|𓃓|};
      case "\196\163\n\230\151\165cca\xF0\x93\x83\x93" {|ģ\n日cca𓃓|};
    ];
  ]

let json_unparse =
  let counter = ref 0 in
  let case jv unparsed =
    let id = get_and_incr counter in
    test_case (sprintf "json_unparse_%d" id) `Quick (fun () ->
        check string
          (sprintf "json_unparsed: %s"
             unparsed)
          unparsed (Json.unparse jv)
      )
  in [
    "json_unparse", [
      case (`null) {|null|};
      case (`bool true) {|true|};
      case (`bool false) {|false|};
      case (`num 3.) {|3|};
      case (`num 0.) {|0|};
      case (`num 3.14) {|3.14|};
      case (`num 4.5e12) {|4.5e+12|};
      case (`num (-3.)) {|-3|};
      case (`num (-4.6e-24)) {|-4.6e-24|};
      case (`str "abc") {|"abc"|};
      case (`str "a\nbc") {|"a\nbc"|};
      case (`str "a\127bc") {|"a\u007fbc"|};
      case (`str "\196\163") {|"ģ"|};
      case (`str "\230\151\165") {|"日"|};
      case (`str "\xF0\x93\x83\x93") {|"𓃓"|};
      case (`str "\196\163\n\230\151\165cca\xF0\x93\x83\x93") {|"ģ\n日cca𓃓"|};
      case (`arr []) {|[]|};
      case (`obj []) {|{}|};
      case (`arr [`obj []; `bool true]) {|[{},true]|};
      case (`arr [`null]) {|[null]|};
      case (`arr [`null;`null;`null]) {|[null,null,null]|};
      case (`obj ["num", `num 1.]) {|{"num":1}|};
      case (`obj ["_1_num", `num 1.; "_2_arr", `arr [`null;`str "hello"]]
            |> Json.normalize)
        {|{"_1_num":1,"_2_arr":[null,"hello"]}|};
    ];
  ]

let string_ignore_whitespace =
  let normalize s =
    let buf = Buffer.create (String.length s) in
    let addc = Buffer.add_char buf in
    let last = ref `whitespace in
    s |> String.iter (function
         | (' ' | '\n' | '\t') when !last = `whitespace -> ()
         | (' ' | '\n' | '\t') (* otherwise *) ->
            last := `whitespace;
            addc ' '
         | c ->
            last := `non_whitespace;
            addc c);
    Buffer.contents buf in
  let pp_string ppf x = fprintf ppf "%S" x in
  testable pp_string (fun a b -> normalize a = normalize b)

let json_show =
  let counter = ref 0 in
  let case jv shown =
    let id = get_and_incr counter in
    test_case (sprintf "json_show_%d" id) `Quick (fun () ->
        check string_ignore_whitespace
          (sprintf "json_show: %s"
             shown)
          shown (Json.show jv)
      )
  in [
    "json_show", [
      case (`null) {|`null|};
      case (`bool true) {|`bool true|};
      case (`bool false) {|`bool false|};
      case (`num 3.) {|`num 3.|};
      case (`num 0.) {|`num 0.|};
      case (`num 3.14) {|`num 3.14|};
      case (`num 4.5e12) {|`num 4.5e+12|};
      case (`num (-0.)) {|`num 0.|};
      case (`num 0.) {|`num 0.|};
      case (`num (-3.)) {|`num (-3.)|};
      case (`num (-4.6e-24)) {|`num (-4.6e-24)|};
      case (`str "abc") {|`str "abc"|};
      case (`str "a\nbc") {|`str "a\nbc"|};
      case (`str "a\127bc") {|`str "a\127bc"|};
      case (`str "\196\163") {|`str "\196\163"|};
      case (`str "\230\151\165") {|`str "\230\151\165"|};
      case (`str "\xF0\x93\x83\x93") {|`str "\240\147\131\147"|};
      case (`str "\196\163\n\230\151\165cca\xF0\x93\x83\x93") {|`str "\196\163\n\230\151\165cca\240\147\131\147"|};
      case (`arr []) {|`arr []|};
      case (`obj []) {|`obj []|};
      case (`arr [`obj []; `bool true]) {|`arr [`obj []; `bool true]|};
      case (`arr [`null]) {|`arr [`null]|};
      case (`arr [`null;`null;`null]) {|`arr [`null; `null; `null]|};
      case (`obj ["num", `num 1.]) {|`obj ["num", `num 1.]|};
      case (`obj ["_1_num", `num 1.; "_2_arr", `arr [`null;`str "hello"]]
            |> Json.normalize)
        {|`obj ["_1_num", `num 1.; "_2_arr", `arr [`null; `str "hello"]]|};
    ];
  ]

let () =
  Printexc.record_backtrace true;
  run "Datecode_unit_tests" ([
    "trivial", [
      test_case "trivial_case" `Quick trivial
    ];
    "seq_iteri", [
      test_case "seq_iteri_0" `Quick seq_iteri_0
    ];
    "seq_range", [
      test_case "seq_range_0" `Quick seq_range_0
    ];
    "seq_make", [
      test_case "seq_make_0" `Quick seq_make_0;
      test_case "seq_make_1" `Quick seq_make_1;
      test_case "seq_make_2" `Quick seq_make_2;
      test_case "seq_make_3" `Quick seq_make_3;
    ];
    "seq_take", [
      test_case "seq_take_0" `Quick seq_take_0;
      test_case "seq_take_1" `Quick seq_take_1
    ];
    "seq_drop", [
      test_case "seq_drop_0" `Quick seq_drop_0;
      test_case "seq_drop_1" `Quick seq_drop_1;
      test_case "seq_drop_2" `Quick seq_drop_2
    ];
  ] @ stream_suite
    @ json_escaped_suite
    @ json_unparse @ json_show
  @ [
    "base64", [
      test_case "base64_known" `Quick base64_known;
      test_case "base64_range" `Quick base64_range;
      test_case "base64_decode_pad" `Quick base64_decode_pad;
    ];

    "json_of_jsonm", [
      test_case "json_of_jsonm_null" `Quick json_of_jsonm_null;
      test_case "json_of_jsonm_bool_0" `Quick json_of_jsonm_bool_0;
      test_case "json_of_jsonm_bool_1" `Quick json_of_jsonm_bool_1;
      test_case "json_of_jsonm_num_0" `Quick json_of_jsonm_num_0;
      test_case "json_of_jsonm_num_1" `Quick json_of_jsonm_num_1;
      test_case "json_of_jsonm_num_2" `Quick json_of_jsonm_num_2;
      test_case "json_of_jsonm_str_0" `Quick json_of_jsonm_str_0;
      test_case "json_of_jsonm_str_1" `Quick json_of_jsonm_str_1;
      test_case "json_of_jsonm_str_2" `Quick json_of_jsonm_str_2;
      test_case "json_of_jsonm_arr_0" `Quick json_of_jsonm_arr_0;
      test_case "json_of_jsonm_arr_1" `Quick json_of_jsonm_arr_1;
      test_case "json_of_jsonm_arr_2" `Quick json_of_jsonm_arr_2;
      test_case "json_of_jsonm_arr_3" `Quick json_of_jsonm_arr_3;
      test_case "json_of_jsonm_arr_4" `Quick json_of_jsonm_arr_4;
      test_case "json_of_jsonm_arr_5" `Quick json_of_jsonm_arr_5;
      test_case "json_of_jsonm_arr_6" `Quick json_of_jsonm_arr_6;
      test_case "json_of_jsonm_obj_0" `Quick json_of_jsonm_obj_0;
      test_case "json_of_jsonm_obj_1" `Quick json_of_jsonm_obj_1;
      test_case "json_of_jsonm_obj_2" `Quick json_of_jsonm_obj_2;
      test_case "json_of_jsonm_obj_3" `Quick json_of_jsonm_obj_3;
      test_case "json_of_jsonm_obj_4" `Quick json_of_jsonm_obj_4;
      test_case "json_of_jsonm_obj_5" `Quick json_of_jsonm_obj_5;
    ];

    "Json.access", jvpath_access_cases;
  ])
