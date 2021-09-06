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


let () =
  Printexc.record_backtrace true;
  run "Datecode_unit_tests" [
      "trivial", [
        test_case "trivial_case" `Quick trivial
      ];
      "seq_iteri", [
          test_case "seq_iteri_0" `Quick seq_iteri_0
      ]
    ]
