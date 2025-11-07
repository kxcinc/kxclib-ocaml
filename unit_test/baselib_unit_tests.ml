open Alcotest

let exn =
  let fmt ppf e = Format.pp_print_string ppf (Printexc.to_string e) in
  testable fmt (=)

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

let string_partition =
  let counter = ref 0 in
  let case str n (p1, p2) =
    let id = get_and_decr counter |> (~-) in
    test_case (sprintf "string_partition_%d" id) `Quick (fun () ->
        check (option (pair string string))
          (sprintf "string_partition_opt.case %S %d (%S, %S)"
             str n p1 p2)
          (Some (p1, p2))
          (String.partition_opt n str);
        check (pair string string)
          (sprintf "string_partition_exn.case %S %d (%S, %S)"
             str n p1 p2)
          (p1, p2)
          (String.partition n str);
      ) in
  let case_fail str n = (* failure cases *)
    let id = get_and_decr counter |> (~-) in
    test_case (sprintf "string_partition_%d" id) `Quick (fun () ->
        check (option (pair string string))
          (sprintf "string_partition_opt.should_fail %S %d"
             str n)
          (None)
          (String.partition_opt n str);
        try
          let p1, p2 = String.partition n str in
          failf "string_partition_exn.should_fail %S %d but got (%S, %S)"
            str n p1 p2
        with Invalid_argument _ -> ()
      )
  in [
    "string_partition", [
      case "" 0 ("", "");
      case_fail "" 1;
      case "a" 0 ("", "a");
      case "a" 1 ("a", "");
      case_fail "a" 2;
      case "hello" 0 ("", "hello");
      case "hello" 3 ("hel", "lo");
      case "hello" 5 ("hello", "");
      case_fail "hello" 7;
    ];
  ]


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
  let wrong_padding = Either.left (Invalid_argument "Base64.decode: wrong padding") in
  let wrong_padding_len len = Either.left (Base64.Invalid_base64_padding (`invalid_padding_length len)) in
  let right = Either.right in
  let input_cases = [
    "Zm8", wrong_padding;
    "Zm8=", right "fo";
    "Zm8==", wrong_padding;
    "Zm8===", wrong_padding;
    "Zm8=====", wrong_padding_len 5;
    "Zm9v", right "foo";
    "Zm9v====", wrong_padding_len 4;
    "Zg", wrong_padding;
    "Zg=", wrong_padding;
    "Zg==", right "f";
    "Zg===", wrong_padding;
    "Zg==\n", right "f";
    "Zg\n==\n", right "f";
    "Z\ng\n=\n=\n", right "f";
    "Zm9vYmE=", right "fooba";
    "Zm9v\nYmE=", right "fooba";
    "Zm9v\nYmE=\n", right "fooba";
    "Zm9v\nYmE", wrong_padding;
    "Zm9v\nYmE\n", wrong_padding;
  ] in
  input_cases |> List.iter (fun (input_case, expected) ->
    match expected with
    | Either.Right expected_value ->
      let actual_plain_bytes = Base64.decode input_case in
      let actual_plain_string = Bytes.to_string actual_plain_bytes in
      check string input_case expected_value actual_plain_string
    | Either.Left expected_exn ->
      try
        let _ = Base64.decode input_case in
        fail "Invalid_argument expected"
      with
      | e -> check exn input_case expected_exn e
  )

let urlenc_known () =
  let bidirectional_cases = [
      "a", "a";
      "abc", "abc";
      "~abc", "~abc";
      "_", "_";
      "kxclib.ml", "kxclib.ml";
      "kxc.dev/kxclib", "kxc.dev%2Fkxclib";
      "kxc\xaa.~", "kxc%AA.~";
    ] in
  let decode_only_cases = [
      "kxc\xaa.~", "kxc%aa.~";
      "kxc.dev/kxclib", "kxc.dev%2fkxclib";
    ] in
  bidirectional_cases
  |> List.iter (fun (raw, encoded) ->
         check string (sprintf "urlenc_encode: %S" raw)
           encoded (Url_encoding.encode (Bytes.of_string raw));
         check string (sprintf "urlenc_decode: %S" raw)
           raw (Url_encoding.decode encoded |> Bytes.to_string);
       );
  decode_only_cases
  |> List.iter (fun (raw, encoded) ->
         check string (sprintf "urlenc_decode: %S" raw)
           raw (Url_encoding.decode encoded |> Bytes.to_string);
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
      case "\\" {|\\|};
      case "\"" {|\"|};
      case "\x1F" {|\u001f|};
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
      case (`str "\000") {|"\u0000"|};
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

let json_unparse_jcsnafi =
  let counter = ref 0 in
  let case jv unparsed_jcsnafi id =
    test_case (sprintf "json_unparse_jcsnafi_%d: %s" id unparsed_jcsnafi) `Quick (fun () ->
        check string
          (sprintf "json_unparsed_jcsnafi: %s"
             unparsed_jcsnafi)
          unparsed_jcsnafi (Json_JCSnafi.unparse_jcsnafi jv)
    )  in
  let case_exn jv expected_exn id =
    test_case (sprintf "json_unparse_jcsnafi_%d" id) `Quick (fun () ->
        check_raises "json_unparsed_jcsnafi should raise exn"
          expected_exn
          (fun () -> ignore (Json_JCSnafi.unparse_jcsnafi jv))
    ) in
  let min_fi_float = Float.of_int (- (1 lsl 52)) in
  let max_fi_float = (Float.of_int ((1 lsl 52) - 1)) in
   [
    "json_unparse_jcsnafi", [
      (* literal case *)
      case (`null) {|null|};
      case (`bool true) {|true|};
      case (`bool false) {|false|};

      (* string case *)
      case (`str "\u{20ac}") {|"€"|};
      case (`str "$")	{|"$"|};	
      case (`str "\u{000F}") {|"\u000f"|};
      case (`str "\u{000a}") {|"\n"|}; (* "\x0A" *)
      case (`str "A") {|"A"|};	
      case (`str "'") {|"'"|};
      case (`str "\u{0042}") {|"B"|};
      case (`str "\u{0022}") {|"\""|}; (* "\x22" *)
      case (`str "\u{005c}") {|"\\"|}; (* "\x5C" *)
      case (`str "\\") {|"\\"|};
      case (`str "\"") {|"\""|};
      case (`str "/") {|"/"|};
      case (`str "\x08") {|"\b"|};
      case (`str "\x09") {|"\t"|};
      case (`str "\x0C") {|"\f"|};
      case (`str "\x0D") {|"\r"|};

      (* Boundary of 1-byte characters |00..7F| *)
      case (`str "\x00") {|"\u0000"|};
      case (`str "\x7F") "\"\x7F\"";
      case_exn (`str "\x80") (Invalid_argument "Invalid Unicode: \x80");

      (* Boundary of 2-byte characters |C2..DF|80..BF| *)
      (*   1st byte check |C2..DF|<valid>| *)
      case_exn (`str "\xC1\x80") (Invalid_argument "Invalid Unicode: \xC1\x80");
      case (`str "\xC2\x80") "\"\xC2\x80\"";
      case (`str "\xDF\x80") "\"\xDF\x80\"";
      case_exn (`str "\xE0\x80") (Invalid_argument "Invalid Unicode: \xE0\x80");
      (*   2st byte check |C2 and DF|80..BF| *)
      case_exn (`str "\xC2\x7F") (Invalid_argument "Invalid Unicode: \xC2\x7F");
      case (`str "\xC2\xBF") "\"\xC2\xBF\"";
      case_exn (`str "\xC2\xC0") (Invalid_argument "Invalid Unicode: \xC2\xC0");
      case_exn (`str "\xDF\x7F") (Invalid_argument "Invalid Unicode: \xDF\x7F");
      case (`str "\xDF\xBF") "\"\xDF\xbf\"";
      case_exn (`str "\xDF\xC0") (Invalid_argument "Invalid Unicode: \xDF\xc0");

      (* Boundary of 3-byte characters |E0..EF|80..BF^|80..BF| *)
      (*   1st byte check |E0..EF|<valid>|<valid>| *)
      case_exn (`str "\xDF\x80\x80") (Invalid_argument "Invalid Unicode: \xDF\x80\x80");
      case (`str "\xE0\xA0\x80") "\"\xE0\xA0\x80\""; (* ^if 1st byte is E0, 2nd byte must be outside the range E0..9F. *)
      case (`str "\xEF\xBF\xBF") "\"\xEF\xBF\xBF\"";
      case_exn (`str "\xF0\x80\x80") (Invalid_argument "Invalid Unicode: \xF0\x80\x80");
      (*   2nd byte check |<valid>|80..BF|<valid>|*)
      case_exn (`str "\xE1\x7F\x80") (Invalid_argument "Invalid Unicode: \xE1\x7F\x80");
      case (`str "\xE1\x80\x80") "\"\xE1\x80\x80\""; (* include checking 3rd byte *)
      case (`str "\xE1\xBF\x80") "\"\xE1\xBF\x80\"";
      case_exn (`str "\xE1\xC0\x80") (Invalid_argument "Invalid Unicode: \xE1\xC0\x80");
      (*   3rd byte check |<valid>|<valid>|80..BF|*)
      case_exn (`str "\xE1\x80\x7F") (Invalid_argument "Invalid Unicode: \xE1\x80\x7F");
      case (`str "\xE1\x80\xBF") "\"\xE1\x80\xBF\"";
      case_exn (`str "\xE1\x80\xC0") (Invalid_argument "Invalid Unicode: \xE1\x80\xC0");
      (*   3-byte characters special check *)
      case_exn (`str "\xE0\x9F\xBF") (Invalid_argument "Invalid Unicode: \xE0\x9F\xBF");
      case (`str "\xED\x9F\xBF") "\"\xED\x9F\xBF\"";
      case_exn (`str "\xED\xA0\x80") (Invalid_argument "Invalid Unicode: \xED\xA0\x80");
      case_exn (`str "\xED\xBF\xBF") (Invalid_argument "Invalid Unicode: \xED\xBF\xBF");
      case (`str "\xEE\x80\x80") "\"\xEE\x80\x80\"";

      (* Boundary of 4-byte characters |F0..F4|80..BF^|80..BF|80..BF| *)
      (*   1st byte check |F0..F4|<valid>|<valid>|<valid>| *)
      case_exn (`str "\xEF\xBF\xBF\xBF") (Invalid_argument "Invalid Unicode: \xEF\xBF\xBF\xBF");
      case (`str "\xF0\x90\x80\x80") "\"\xF0\x90\x80\x80\""; (* ^if 1st byte is F0, 2nd byte is invalid if if it falls within the range 80..8F. *)
      case (`str "\xF4\x8F\xBF\xBF") "\"\xF4\x8F\xBF\xBF\""; (* ^if 1st byte is F4, 2nd byte is invalid if it is 90 or greater. *)
      case_exn (`str "\xF5\x80\x80\x80") (Invalid_argument "Invalid Unicode: \xF5\x80\x80\x80");
      (*   2nd byte check |<valid>|80..BF|<valid>|<valid>| *)
      case_exn (`str "\xF1\x7F\x80\x80") (Invalid_argument "Invalid Unicode: \xF1\x7F\x80\x80");
      case (`str "\xF1\x80\x80\x80") "\"\xF1\x80\x80\x80\""; (* include checking 3rd and 4th byte *)
      case (`str "\xF1\xBF\xBF\xBF") "\"\xF1\xBF\xBF\xBF\""; (* include checking 3rd and 4th byte *)
      case_exn (`str "\xF1\xC0\x80\x80") (Invalid_argument "Invalid Unicode: \xF1\xC0\x80\x80");
      (*   3rd byte check |<valid>|<valid>|80..BF|<valid>| *)
      case_exn (`str "\xF1\x80\x7F\x80") (Invalid_argument "Invalid Unicode: \xF1\x80\x7F\x80");
      case_exn (`str "\xF1\x80\xC0\x80") (Invalid_argument "Invalid Unicode: \xF1\x80\xC0\x80");
      (*   4th byte check |<valid>|<valid>|<valid>|80..BF| *)
      case_exn (`str "\xF1\x80\x80\x7F") (Invalid_argument "Invalid Unicode: \xF1\x80\x80\x7F");
      case_exn (`str "\xF1\x80\x80\xC0") (Invalid_argument "Invalid Unicode: \xF1\x80\x80\xC0");
      (*   4-byte characters special check *)
      case_exn (`str "\xF0\x8F\xBF\xBF") (Invalid_argument "Invalid Unicode: \xF0\x8F\xBF\xBF");
      case_exn (`str "\xF4\x90\x80\x80") (Invalid_argument "Invalid Unicode: \xF4\x90\x80\x80");

      (* number case *)
      case (`num min_fi_float) {|-4503599627370496|};
      case (`num max_fi_float) {|4503599627370495|};
      case (`num (-0.)) {|0|};
      case (`num 0.) {|0|};
      case (`num (+0.)) {|0|};
      case_exn (`num (Float.pred min_fi_float)) (Invalid_argument "Number cannot be safely encoded with Json_JSCnafi (encountering: -4503599627370497.000000)");
      case_exn (`num (Float.succ max_fi_float)) (Invalid_argument "Number cannot be safely encoded with Json_JSCnafi (encountering: 4503599627370495.500000)");
      case_exn (`num (-1.5)) (Invalid_argument "Number cannot be safely encoded with Json_JSCnafi (encountering: -1.500000)");
      case_exn (`num 4.8) (Invalid_argument "Number cannot be safely encoded with Json_JSCnafi (encountering: 4.800000)");

      (* object case *)
      case (`obj []) {|{}|};
      case (`obj [("null", `null)]) {|{"null":null}|};
      case (`obj [("boolean", `bool true)]) {|{"boolean":true}|};
      case (`obj [("boolean", `bool false)]) {|{"boolean":false}|};
      case (`obj [("string", `str "foo")]) {|{"string":"foo"}|};
      case (`obj [("string", `str "あ")]) {|{"string":"あ"}|};
      case (`obj [("string", `str "\u{20ac}")]) {|{"string":"€"}|};
      case (`obj [("string", `str "$")])	{|{"string":"$"}|};	
      case (`obj [("string", `str "\u{000F}")]) {|{"string":"\u000f"}|};
      case (`obj [("string", `str "\u{000a}")]) {|{"string":"\n"}|};
      case (`obj [("string", `str "A")]) {|{"string":"A"}|};	
      case (`obj [("string", `str "'")]) {|{"string":"'"}|};
      case (`obj [("string", `str "\u{0042}")]) {|{"string":"B"}|};
      case (`obj [("string", `str "\u{0022}")]) {|{"string":"\""}|};
      case (`obj [("string", `str "\u{005c}")]) {|{"string":"\\"}|};
      case (`obj [("string", `str "\\")]) {|{"string":"\\"}|};
      case (`obj [("string", `str "\"")]) {|{"string":"\""}|};
      case (`obj [("string", `str "/")]) {|{"string":"/"}|};
      case (`obj [("あ", `null)]) {|{"あ":null}|};
      case (`obj [("\u{20ac}", `null)]) {|{"€":null}|};
      case (`obj [("$", `null)])	{|{"$":null}|};	
      case (`obj [("\u{000F}", `null)]) {|{"\u000f":null}|};
      case (`obj [("\u{000a}", `null)]) {|{"\n":null}|};
      case (`obj [("A", `null)]) {|{"A":null}|};	
      case (`obj [("'", `null)]) {|{"'":null}|};
      case (`obj [("\u{0042}", `null)]) {|{"B":null}|};
      case (`obj [("\u{0022}", `null)]) {|{"\"":null}|};
      case (`obj [("\u{005c}", `null)]) {|{"\\":null}|};
      case (`obj [("\\", `null)]) {|{"\\":null}|};
      case (`obj [("\"", `null)]) {|{"\"":null}|};
      case (`obj [("/", `null)]) {|{"/":null}|};
      case (`obj [("number", `num 1.0)]) {|{"number":1}|};
      case (`obj [("null", `null); ("boolean", `bool true); ("string", `str "foo"); ("number", `num 1.0)])
           {|{"boolean":true,"null":null,"number":1,"string":"foo"}|};
      case (`obj [("obj", `obj [("name", `str "foo"); ("age", `num 30.0)])]) {|{"obj":{"age":30,"name":"foo"}}|};
      case (`obj [("array", `arr [])]) {|{"array":[]}|};
      case (`obj [("array", `arr [`null])]) {|{"array":[null]}|};
      case (`obj [("array", `arr [`bool true; `bool false])]) {|{"array":[true,false]}|};
      case (`obj [("array", `arr [`null; `bool true; `bool false; `str "foo"; `num 1.0])])
           {|{"array":[null,true,false,"foo",1]}|};
      case_exn (`obj [("foo", `bool true); ("foo", `bool false)]) (Invalid_argument "Duplicate property names: foo");
      case_exn (`obj [("あ", `bool true); ("あ", `bool false)])  (Invalid_argument "Duplicate property names: あ");
      case_exn (`obj [("あいう", `bool true); ("あいう", `bool false)])  (Invalid_argument "Duplicate property names: あいう");
      case_exn (`obj [("\u{20ac}", `bool true); ({|€|}, `bool false)]) (Invalid_argument "Duplicate property names: €");

      (* array case *)
      case (`arr []) {|[]|};
      case (`arr [`null]) {|[null]|};
      case (`arr [`bool true]) {|[true]|};
      case (`arr [`bool false]) {|[false]|};
      case (`arr [`str "foo"]) {|["foo"]|};
      case (`arr [`str "あ"]) {|["あ"]|};
      case (`arr [`str "foo"; `str "あ"; `str "\u{20ac}"; `str "$"; `str "\u{000F}"; `str "\u{000a}"; `str "A"; `str "'"; `str "\u{0042}"; `str "\u{0022}"; `str "\u{005c}"; `str "\\"; `str "\""; `str "/"])
          {|["foo","あ","€","$","\u000f","\n","A","'","B","\"","\\","\\","\"","/"]|};
      case (`arr [`str "fooあ\u{20ac}$\u{000F}\u{000a}A'\u{0042}\u{0022}\u{005c}\\\"/"])
          {|["fooあ€$\u000f\nA'B\"\\\\\"/"]|};
      case (`arr [`num 1.0]) {|[1]|};
      case (`arr [`num (-1.0)]) {|[-1]|};
      case_exn (`arr [`num 2.3]) (Invalid_argument "Number cannot be safely encoded with Json_JSCnafi (encountering: 2.300000)");
      case_exn (`arr [`num (-5.0); `num 2.3]) (Invalid_argument "Number cannot be safely encoded with Json_JSCnafi (encountering: 2.300000)");
      case_exn (`arr [`num 2.3; `num (-5.0)]) (Invalid_argument "Number cannot be safely encoded with Json_JSCnafi (encountering: 2.300000)");
      case (`arr [`obj []]) {|[{}]|};
      case (`arr [`obj [("null", `null)]]) {|[{"null":null}]|};
      case (`arr [`obj [("boolean", `bool true)]]) {|[{"boolean":true}]|};
      case (`arr [`obj [("boolean", `bool false)]]) {|[{"boolean":false}]|};
      case (`arr [`obj [("string", `str "foo")]]) {|[{"string":"foo"}]|};
      case (`arr [`obj [("string", `str "あ")]]) {|[{"string":"あ"}]|};
      case (`arr [`obj [("string", `str "\u{20ac}")]]) {|[{"string":"€"}]|};
      case (`arr [`obj [("string", `str "$")]])	{|[{"string":"$"}]|};	
      case (`arr [`obj [("string", `str "\u{000F}")]]) {|[{"string":"\u000f"}]|};
      case (`arr [`obj [("string", `str "\u{000a}")]]) {|[{"string":"\n"}]|};
      case (`arr [`obj [("string", `str "A")]]) {|[{"string":"A"}]|};	
      case (`arr [`obj [("string", `str "'")]]) {|[{"string":"'"}]|};
      case (`arr [`obj [("string", `str "\u{0042}")]]) {|[{"string":"B"}]|};
      case (`arr [`obj [("string", `str "\u{0022}")]]) {|[{"string":"\""}]|};
      case (`arr [`obj [("string", `str "\u{005c}")]]) {|[{"string":"\\"}]|};
      case (`arr [`obj [("string", `str "\\")]]) {|[{"string":"\\"}]|};
      case (`arr [`obj [("string", `str "\"")]]) {|[{"string":"\""}]|};
      case (`arr [`obj [("string", `str "/")]]) {|[{"string":"/"}]|};
      case (`arr [`obj [("あ", `null)]]) {|[{"あ":null}]|};
      case (`arr [`obj [("\u{20ac}", `null)]]) {|[{"€":null}]|};
      case (`arr [`obj [("$", `null)]])	{|[{"$":null}]|};	
      case (`arr [`obj [("\u{000F}", `null)]]) {|[{"\u000f":null}]|};
      case (`arr [`obj [("\u{000a}", `null)]]) {|[{"\n":null}]|};
      case (`arr [`obj [("A", `null)]]) {|[{"A":null}]|};	
      case (`arr [`obj [("'", `null)]]) {|[{"'":null}]|};
      case (`arr [`obj [("\u{0042}", `null)]]) {|[{"B":null}]|};
      case (`arr [`obj [("\u{0022}", `null)]]) {|[{"\"":null}]|};
      case (`arr [`obj [("\u{005c}", `null)]]) {|[{"\\":null}]|};
      case (`arr [`obj [("\\", `null)]]) {|[{"\\":null}]|};
      case (`arr [`obj [("\"", `null)]]) {|[{"\"":null}]|};
      case (`arr [`obj [("/", `null)]]) {|[{"/":null}]|};
      case (`arr [`obj [("number", `num 1.0)]]) {|[{"number":1}]|};
      case (`arr [`obj [("null", `null); ("boolean", `bool true); ("string", `str "foo"); ("number", `num 1.0)]])
           {|[{"boolean":true,"null":null,"number":1,"string":"foo"}]|};
      case (`arr [`obj [("obj", `obj [("name", `str "foo"); ("age", `num 30.0)])]]) {|[{"obj":{"age":30,"name":"foo"}}]|};
      case (`arr [`null; `bool true; `bool false; `num 1.0; `num (-1.0); `str "foo"; `str "あ"; `obj [("obj", `obj [("name", `str "foo"); ("age", `num 30.0)])]])
           {|[null,true,false,1,-1,"foo","あ",{"obj":{"age":30,"name":"foo"}}]|};
      case (`arr [`arr []]) "[[]]";
      case (`arr [`arr [`bool true; `bool false]]) {|[[true,false]]|};
      case (`arr [`arr [`bool true; `bool false]; `arr [`num 2.0; `num (-5.0)]]) {|[[true,false],[2,-5]]|};
      case (`arr [`arr [`bool true; `bool false]; `arr [`num 2.0; `num (-5.0)]; `arr [`obj [("name", `str "foo"); ("age", `num 30.0)]; `obj [("name", `str "bar"); ("age", `num 23.0)]]])
      {|[[true,false],[2,-5],[{"age":30,"name":"foo"},{"age":23,"name":"bar"}]]|};

      (* RFC 8785, sec3.2.2 for jcsnafi*)
      case (`obj [ ("numbers", `arr [`num 333333333.0; `num 4.0; `num 2e+3; `num 0.0]);
                   ("string", `str "\u{20ac}$\u{000F}\u{000a}A'\u{0042}\u{0022}\u{005c}\\\"/");
                   ("literals", `arr [`null; `bool true; `bool false])])
           {|{"literals":[null,true,false],"numbers":[333333333,4,2000,0],"string":"€$\u000f\nA'B\"\\\\\"/"}|};

      (* RFC 8785, sec3.2.2 original *)
      case_exn (`obj [ ("numbers", `arr [`num 333333333.33333329; `num 1E30; `num 4.50; `num 2e-3; `num 0.000000000000000000000000001]);
                   ("string", `str "\u{20ac}$\u{000F}\u{000a}A'\u{0042}\u{0022}\u{005c}\\\"/");
                   ("literals", `arr [`null; `bool true; `bool false])])
               (Invalid_argument "Number cannot be safely encoded with Json_JSCnafi (encountering: 333333333.333333)");
               (* {|{"literals":[null,true,false],"numbers":[333333333.3333333,1e+30,4.5,0.002,1e-27],"string":"€$\u000f\nA'B\"\\\\\"/"}|}; *)

    ] |&> (fun case -> get_and_incr counter |> case)
  ]

let jcsnafi_is_encodable_str = 
  let counter = ref 0 in
  let case s expected_value id =
    test_case (sprintf "jcsnafi_is_encodable_str_%d:" id) `Quick (fun () ->
        check bool
          (sprintf "jcsnafi_is_encodable_str:")
          expected_value (Json_JCSnafi.is_encodable_str s)
    ) in
   [
    "jcsnafi_is_encodable_str", [
      case "\u{20ac}" true;
      case "$" true;	
      case "\u{000F}" true;
      case "\u{000a}" true; (* "\x0A" *)
      case "A" true;	
      case "'" true;
      case "\u{0042}" true;
      case "\u{0022}" true; (* "\x22" *)
      case "\u{005c}" true; (* "\x5C" *)
      case "\\" true;
      case "\"" true;
      case "/" true;
      case "\x08" true;
      case "\x09" true;
      case "\x0C" true;
      case "\x0D" true;

      (* Boundary of 1-byte characters |00..7F| *)
      case "\x00" true;
      case "\x7F" true;
      case "\x80" false;

      (* Boundary of 2-byte characters |C2..DF|80..BF| *)
      (*   1st byte check |C2..DF|<valid>| *)
      case "\xC1\x80" false;
      case "\xC2\x80" true;
      case "\xDF\x80" true;
      case "\xE0\x80" false;
      (*   2st byte check |C2 and DF|80..BF| *)
      case "\xC2\x7F" false;
      case "\xC2\xBF" true;
      case "\xC2\xC0" false;
      case "\xDF\x7F" false;
      case "\xDF\xBF" true;
      case "\xDF\xC0" false;

      (* Boundary of 3-byte characters |E0..EF|80..BF^|80..BF| *)
      (*   1st byte check |E0..EF|<valid>|<valid>| *)
      case "\xDF\x80\x80" false;
      case "\xE0\xA0\x80" true; (* ^if 1st byte is E0, 2nd byte must be outside the range E0..9F. *)
      case "\xEF\xBF\xBF" true;
      case "\xF0\x80\x80" false;
      (*   2nd byte check |<valid>|80..BF|<valid>|*)
      case "\xE1\x7F\x80" false;
      case "\xE1\x80\x80" true; (* include checking 3rd byte *)
      case "\xE1\xBF\x80" true;
      case "\xE1\xC0\x80" false;
      (*   3rd byte check |<valid>|<valid>|80..BF|*)
      case "\xE1\x80\x7F" false;
      case "\xE1\x80\xBF" true;
      case "\xE1\x80\xC0" false;
      (*   3-byte characters special check *)
      case "\xE0\x9F\xBF" false;
      case "\xED\x9F\xBF" true;
      case "\xED\xA0\x80" false;
      case "\xED\xBF\xBF" false;
      case "\xEE\x80\x80" true;

      (* Boundary of 4-byte characters |F0..F4|80..BF^|80..BF|80..BF| *)
      (*   1st byte check |F0..F4|<valid>|<valid>|<valid>| *)
      case "\xEF\xBF\xBF\xBF" false;
      case "\xF0\x90\x80\x80" true; (* ^if 1st byte is F0, 2nd byte is invalid if if it falls within the range 80..8F. *)
      case "\xF4\x8F\xBF\xBF" true; (* ^if 1st byte is F4, 2nd byte is invalid if it is 90 or greater. *)
      case "\xF5\x80\x80\x80" false;
      (*   2nd byte check |<valid>|80..BF|<valid>|<valid>| *)
      case "\xF1\x7F\x80\x80" false;
      case "\xF1\x80\x80\x80" true; (* include checking 3rd and 4th byte *)
      case "\xF1\xBF\xBF\xBF" true; (* include checking 3rd and 4th byte *)
      case "\xF1\xC0\x80\x80" false;
      (*   3rd byte check |<valid>|<valid>|80..BF|<valid>| *)
      case "\xF1\x80\x7F\x80" false;
      case "\xF1\x80\xC0\x80" false;
      (*   4th byte check |<valid>|<valid>|<valid>|80..BF| *)
      case "\xF1\x80\x80\x7F" false;
      case "\xF1\x80\x80\xC0" false;
      (*   4-byte characters special check *)
      case "\xF0\x8F\xBF\xBF" false;
      case "\xF4\x90\x80\x80" false;
    ] |&> (fun case -> get_and_incr counter |> case)
  ]

  let jcsnafi_is_encodable_num = 
  let counter = ref 0 in
  let case f expected_value id =
    test_case (sprintf "jcsnafi_is_encodable_num_%d:" id) `Quick (fun () ->
        check bool
          (sprintf "jcsnafi_is_encodable_num:")
          expected_value (Json_JCSnafi.is_encodable_num f)
    ) in
  let min_fi_float = Float.of_int (- (1 lsl 52)) in
  let max_fi_float = (Float.of_int ((1 lsl 52) - 1)) in
   [
    "jcsnafi_is_encodable_num", [
      case min_fi_float true;
      case max_fi_float true;
      case (-0.) true;
      case (0.) true;
      case (+0.) true;
      case (Float.pred min_fi_float) false;
      case (Float.succ max_fi_float) false;
      case (-1.5) false;
      case 4.8 false;
    ] |&> (fun case -> get_and_incr counter |> case)
  ]

let jcsnafi_compare_field_name = 
  let counter = ref 0 in
  let case str1 str2 expected_value id =
    test_case (sprintf "jcsnafi_compare_field_name_%d:" id) `Quick (fun () ->
        check int
          (sprintf "jcsnafi_compare_field_name:")
          expected_value (Json_JCSnafi.compare_field_name str1 str2)
    ) in
  [
    "jcsnafi_compare_field_name", [
      case "" "" 0;
      case "" "a" (-1);
      case "a" "" 1;
      case "a" "a" 0;
      case "a" "b" (-1);
      case "b" "a" 1;
      case "aa" "aa" 0;
      case "a" "aa" (-1);
      case "aa" "a" 1;
      case "あ" "あ" 0;
      case "あ" "い" (-1);
      case "い" "あ" 1;

      (* RFC 8785, Sec3.2.3 testcase *)
      case "\r" "1" (-1);
      case "\r" "\u{0080}" (-1);
      case "\r" "\u{00f6}" (-1);
      case "\r" "\u{20ac}" (-1);
      case "\r" "\u{1f600}" (-1);       (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\r" "\u{fb33}" (-1);
      case "\r" "\r" 0;      
      case "1" "\u{0080}" (-1);
      case "1" "\u{00f6}" (-1);
      case "1" "\u{20ac}" (-1);
      case "1" "\u{1f600}" (-1);        (* eqv with surrogate pair \uD83D\uDE00 *)
      case "1" "\u{fb33}" (-1);
      case "1" "1" 0;
      case "1" "\r" 1;
      case "\u{0080}" "\u{00f6}" (-1);
      case "\u{0080}" "\u{20ac}" (-1);
      case "\u{0080}" "\u{1f600}" (-1); (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{0080}" "\u{fb33}" (-1);
      case "\u{0080}" "\u{0080}" 0;
      case "\u{0080}" "\r" 1;
      case "\u{0080}" "1" 1;
      case "\u{00f6}" "\u{20ac}" (-1);
      case "\u{00f6}" "\u{1f600}" (-1); (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{00f6}" "\u{fb33}" (-1);
      case "\u{00f6}" "\u{00f6}" 0;
      case "\u{00f6}" "\r" 1;
      case "\u{00f6}" "1" 1;
      case "\u{00f6}" "\u{0080}" 1;
      case "\u{20ac}" "\u{1f600}" (-1); (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{20ac}" "\u{fb33}" (-1);
      case "\u{20ac}" "\u{20ac}" 0;
      case "\u{20ac}" "\r" 1;
      case "\u{20ac}" "1" 1;
      case "\u{20ac}" "\u{0080}" 1;
      case "\u{20ac}" "\u{00f6}" 1;
      case "\u{1f600}" "\u{fb33}" (-1); (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "\u{1f600}" 0;   (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "\r" 1;          (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "1" 1;           (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "\u{0080}" 1;    (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "\u{00f6}" 1;    (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "\u{20ac}" 1;    (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{fb33}" "\u{fb33}" 0;
      case "\u{fb33}" "\r" 1;
      case "\u{fb33}" "1" 1;
      case "\u{fb33}" "\u{0080}" 1;
      case "\u{fb33}" "\u{00f6}" 1;
      case "\u{fb33}" "\u{20ac}" 1;
      case "\u{fb33}" "\u{1f600}" 1;    (* eqv with surrogate pair \uD83D\uDE00 *)
      
    ] |&> (fun case -> get_and_incr counter |> case)
  ]

let jcsnafi_compare_field_name_rfc8785 =
  let input = [
                ("\u{20ac}", "Euro Sign");
                ("\r", "Carriage Return");
                ("\u{fb33}", "Hebrew Letter Dalet With Dagesh");
                ("1", "One");
                ("\u{1f600}", "Emoji: Grinning Face"); (* eqv with surrogate pair \uD83D\uDE00 *)
                ("\u{0080}", "Control");
                ("\u{00f6}", "Latin Small Letter O With Diaeresis");
              ] in
  let expected = [
                   "Carriage Return";
                   "One";
                   "Control";
                   "Latin Small Letter O With Diaeresis";
                   "Euro Sign";
                   "Emoji: Grinning Face";
                   "Hebrew Letter Dalet With Dagesh"
                 ] in
  [
    "jcsnafi_compare_field_name_rfc8785", [
      test_case (sprintf "jcsnafi_compare_field_name_rfc8785:") `Quick (fun () ->
        check (list string)
          (sprintf "jcsnafi_compare_field_name_rfc8785:")
          expected
          (List.map (fun (_, v) -> v) @@ List.sort (fun (k1, _) (k2, _) -> Json_JCSnafi.compare_field_name k1 k2) input)
      )
    ]
  ]

let jvpath_unparse =
  let counter = ref 0 in
  let case jvpath unparsed id =
    test_case (sprintf "jvpath_unparse_%d: %s" id unparsed) `Quick (fun () ->
        check string (sprintf "jvpath_unparse_check: %s" unparsed)
          unparsed
          (Json.unparse_jvpath jvpath)
      ) in
  ["jvpath_unparse", [
      case [] ".";
      case [`f "foo"] ".foo";
      case [`f "foo"; `f "b!ar"] ".foo[\"b!ar\"]";
      case [`f "foo"; `f "bar"] ".foo.bar";
      case [`f ""] ".[\"\"]";
      case [`f "\000"] ".[\"\\u0000\"]";
      case [`f "foo"; `i 4] ".foo[4]";
      case [`i 3] ".[3]";
      case [`i 3; `i 4] ".[3][4]";
      case [`i 3; `f "bar"] ".[3].bar";
      case [`i 3; `f "b!ar"] ".[3][\"b!ar\"]";
      case [`i 3; `f "in"] ".[3][\"in\"]";
      case [`f "f!oo"] ".[\"f!oo\"]";
      case [`f "f\"oo"] ".[\"f\\\"oo\"]";
  ] |&> (fun case -> get_and_incr counter |> case)]

let jvpath_parse_success =
  let counter = ref 0 in
  let jvpath = testable Json.pp_jvpath (=) in
  let case input path id =
    test_case (sprintf "jvpath_parse_success_%d: %s" id input) `Quick (fun () ->
        check jvpath (sprintf "jvpath_parse: %s" input)
          path
          (Json.parse_jvpath_exn input)
      ) in
  ["jvpath_parse_success", [
      case "." [];
      case " ." [];
      case " . " [];
      case ".foo" [`f "foo"];
      case ".foo " [`f "foo"];
      case ".foo.bar" [`f "foo"; `f "bar"];
      case ".[\"f!oo\"]" [`f "f!oo"];
      case ".[\"f!oo\"] " [`f "f!oo"];
      case ". [\"f!oo\"] " [`f "f!oo"];
      case ".[\"\"]" [`f ""];
      case ".[\"\\u0000\"]" [`f "\000"];
      case ".foo[4]" [`f "foo"; `i 4];
      case ".[3]" [`i 3];
      case ".[3] " [`i 3];
      case ".[3][4]" [`i 3; `i 4];
      case ".[3].bar" [`i 3; `f "bar"];
      case ".[3][\"b!ar\"]" [`i 3; `f "b!ar"];
      case ".[3][\"in\"]" [`i 3; `f "in"];
      case ".foo[\"b!ar\"]" [`f "foo"; `f "b!ar"];
      case " .[3] [4] " [`i 3; `i 4];
      case ".[3] .bar " [`i 3; `f "bar"];
      case ". [3] [\"b!ar\"]" [`i 3; `f "b!ar"];
      case ". [3] [\"in\"]" [`i 3; `f "in"];
  ] |&> (fun case -> get_and_incr counter |> case)]

let jv_pump_fields =
  let counter = ref 0 in
  let pos_case fns j m id =
    let nr = Json.normalize in
    let pumped = Jv.pump_fields fns j in
    test_case (sprintf "jv_pump_fields_pos_%d" id) `Quick (fun () ->
        check jv "pumped eqv original" (nr j) (nr pumped);
        check bool
          (sprintf "pumped pass matcher test; pumped: %a"
             pp_jv pumped)
          true (m pumped))
  in
  let neg_case fns j id =
    let pumped = Jv.pump_fields fns j in
    test_case (sprintf "jv_pump_fields_neg_%d" id) `Quick (fun () ->
        check jv "pumped equal original" j pumped;)
  in
  ["jv_pump_fields", [
      pos_case [] (`obj [])
        (fun _ -> true);
      pos_case [] (`obj ["a", `num 1.; "b", `num 2.; "c", `num 3.])
        (fun _ -> true);
      pos_case ["a"] (`obj ["a", `num 1.; "b", `num 2.; "c", `num 3.]) (
        function `obj (("a", `num 1.) :: _) -> true | _ -> false);
      pos_case ["a"; "b"] (`obj ["a", `num 1.; "b", `num 2.; "c", `num 3.]) (
        function `obj (("a", `num 1.) :: ("b", `num 2.) :: _) -> true | _ -> false);
      pos_case ["c"; "b"] (`obj ["a", `num 1.; "b", `num 2.; "c", `num 3.]) (
        function `obj (("c", `num 3.) :: ("b", `num 2.) :: _) -> true | _ -> false);
      neg_case [] (`num 0.);
      neg_case ["d";] (`obj ["a", `num 1.; "b", `num 2.; "c", `num 3.]);
      neg_case ["a"; "d";] (`obj ["a", `num 1.; "b", `num 2.; "c", `num 3.]);
      neg_case ["d"; "a";] (`obj ["a", `num 1.; "b", `num 2.; "c", `num 3.]);
  ] |&> (fun case -> get_and_incr counter |> case)]

let () =
  Printexc.record_backtrace true;
  run "Kxclib_baselib_unit_tests" ([
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
    @ string_partition
    @ json_escaped_suite
    @ json_unparse @ json_show
    @ json_unparse_jcsnafi
    @ jcsnafi_is_encodable_str
    @ jcsnafi_is_encodable_num
    @ jcsnafi_compare_field_name
    @ jcsnafi_compare_field_name_rfc8785
    @ jvpath_unparse
    @ jvpath_parse_success
    @ jv_pump_fields
  @ [
    "base64", [
      test_case "base64_known" `Quick base64_known;
      test_case "base64_range" `Quick base64_range;
      test_case "base64_decode_pad" `Quick base64_decode_pad;
    ];

    "url_encoding", [
        test_case "urlenc_known" `Quick urlenc_known;
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
