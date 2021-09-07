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
           test_case "v' Some" `Quick (fun() ->
               check int "..." (v' (fun() -> def) (Some nondef)) nondef
             );

           test_case "v' None" `Quick (fun() ->
               check int "..." (v' (fun() -> def) None) def
             );
      ]);

      (* HOMEWORK : write test cases about otherwise *)
      "Option.otherwise",
      (let ow = 1234 in
       let def = 4567 in
       Option.[
          test_case "otherwise Some" `Quick (fun() ->
              check (option int) "..." (otherwise (Some ow) (Some def)) (Some def)
            );

          test_case "otherwise None" `Quick (fun() ->
              check (option int) "..." (otherwise (Some ow) None) (Some ow)
           );
      ]);

      "Option.pp",
      (let x = 1234 in
       Option.[
        test_case "pp None" `Quick (fun() ->
            check string "..." (sprintf "%a" (pp pp_int) None) "None"
          );

        (* HOMEWORK : write test cases about Some(_) *)
        test_case "pp Some" `Quick (fun() ->
            check string "..." (sprintf "%a" (pp pp_int) (Some x)) "Some(1234)"
          );
      ]);
      
      (* HOMEWORK : write test cases about fmap *)
      "Option.fmap",
      Option.[
         test_case "fmap Some" `Quick (fun() ->
             check (option int) "..." (fmap some (Some (Some 10))) (Some 10)
           );
      
         test_case "fmap None" `Quick (fun() ->
             check (option int) "..." (fmap some None) None
           );
      ];
      

      (* HOMEWORK : write test cases about of_bool *)
      "Option.of_bool",
      (
        Option.[
          test_case "of_bool Some" `Quick (fun() ->
              check (option unit) "..." (of_bool true) (Some ())
            );

          test_case "of_bool None" `Quick (fun() ->
              check (option unit) "..." (of_bool false) None
            );
        ]
      )

    ]
