open Alcotest


let sprintf fmt = Format.asprintf fmt


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
  check (array string) "seq_iteri" actual expected


let seq_iteri_0 =
  seq_iteri 3 [|"Hello 0.0!"; "Hello 1.1!"; "Hello 2.2!"|]


let seq_range start end_exclusive expected () =
  let actual = Seq.range start end_exclusive |> Array.of_seq in
  check (array int) "seq_range" actual expected


let seq_range_0 = seq_range 3 7 [|3; 4; 5; 6|]


let seq_make tstbl n x expected () =
  let actual = Seq.make n x |> Array.of_seq in
  check tstbl "seq_make" actual expected


let seq_make_0 = seq_make (array int) 5 2 [|2; 2; 2; 2; 2|]
let seq_make_1 = seq_make (array string) 1 "Hi" [|"Hi"|]
let seq_make_2 = seq_make (array int) 0 2 [||]

(* ??? なんでだめなんでしょう ??? *)
(* let seq_make_2 = seq_make (array float) 0 3.14 [||] *)


let seq_take tstbl n org_lst expected_lst () =
  let actual = List.to_seq org_lst |> Seq.take n in
  let actual_lst = List.of_seq actual in
  check tstbl "seq_take" actual_lst expected_lst


let seq_take_0 = seq_take (list int) 2 [2; 3; 4] [2; 3]
let seq_take_1 = seq_take (list string) 0 ["A"; "B"; "C"] []


let seq_drop tstbl n org_lst expected_lst () =
  let actual = List.to_seq org_lst |> Seq.drop n in
  let actual_lst = List.of_seq actual in
  check tstbl "seq_drop" actual_lst expected_lst


let seq_drop_0 = seq_drop (list int) 2 [2; 3; 4] [4]
let seq_drop_1 = seq_drop (list string) 0 ["A"; "B"; "C"] ["A"; "B"; "C"]
let seq_drop_2 = seq_drop (list string) 3 ["A"; "B"; "C"] []


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
          test_case "seq_make_2" `Quick seq_make_2
      ];
      "seq_take", [
          test_case "seq_take_0" `Quick seq_take_0;
          test_case "seq_take_1" `Quick seq_take_1
      ];
      "seq_drop", [
          test_case "seq_drop_0" `Quick seq_drop_0;
          test_case "seq_drop_1" `Quick seq_drop_1;
          test_case "seq_drop_2" `Quick seq_drop_2
      ]
    ]
