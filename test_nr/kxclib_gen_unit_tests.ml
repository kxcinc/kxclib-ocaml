open Alcotest


let sprintf fmt = Format.asprintf fmt


let trivial () =
  check string "I'm trivial" "hello" "hello"


let () =
  Printexc.record_backtrace true;
  run "Datecode_unit_tests" [
      "trivial", [
        test_case "trivial_case" `Quick trivial
      ]
    ]
