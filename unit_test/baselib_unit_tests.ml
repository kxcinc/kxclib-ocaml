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

let () =
  Printexc.record_backtrace true;
  run "Datecode_unit_tests" [
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
    "stream_take", [
      test_case "stream_take_0" `Quick stream_take_0;
      test_case "stream_take_1" `Quick stream_take_1
    ];
    "stream_drop", [
      test_case "stream_drop_0" `Quick stream_drop_0;
      test_case "stream_drop_1" `Quick stream_drop_1;
      test_case "stream_drop_2" `Quick stream_drop_2
    ];
    "base64", [
      test_case "base64_known" `Quick base64_known;
      test_case "base64_range" `Quick base64_range;
    ]
  ]
