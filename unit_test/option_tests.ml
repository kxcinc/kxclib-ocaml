open Alcotest

let () =
  let filename_base = Filename.basename __FILE__ in
  Printexc.record_backtrace true;
  run filename_base [

      "Option.{v,v'}",
      (let def = 33233 in
       let nondef = 13 in
       Option.[
           test_case "v Some" `Quick (fun() ->
               check int "..." (v def (Some nondef)) nondef
             );
           test_case "v None" `Quick (fun() ->
               check int "..." (v def None) def
             );

           (* HOMEWORK : write test cases about v' *)

      ]);

      (* HOMEWORK : write test cases about otherwise *)

      "Option.pp",
      Option.[
        test_case "pp None" `Quick (fun() ->
            check string "..." "None" (sprintf "%a" (pp pp_int) None)
          )

        (* HOMEWORK : write test cases about Some(_) *)
      ];

      (* HOMEWORK : write test cases about fmap *)

      (* HOMEWORK : write test cases about of_bool *)

    ]
