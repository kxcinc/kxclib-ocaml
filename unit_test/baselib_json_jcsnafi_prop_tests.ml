let min_fi_float = Float.of_int (- (1 lsl 52)) ;;
let max_fi_float = (Float.of_int ((1 lsl 52) - 1)) ;;
let min_fi_int = (- (1 lsl 52)) ;;
let max_fi_int = (1 lsl 52) - 1 ;;

let invalid_neg_int_range = (QCheck2.Gen.int_range min_int (pred min_fi_int))
let invalid_pos_int_range = (QCheck2.Gen.int_range (succ max_fi_int) max_int)

let is_exn exn (f : unit -> unit) : bool =
  try
    f ();
    false
  with
  | e when e = exn -> true
  | _ -> false

let () =
  let that ?(count=200) name =
    QCheck2.Test.make
      ~name ~count in

  let run_tests tests = QCheck_base_runner.run_tests_main tests in
  [
    that "is_encodable_num: Invalid negative integer range" invalid_neg_int_range ~print:string_of_int
      (fun i ->
        float_of_int i |> Json_JCSnafi.is_encodable_num |> not
      );
    that "is_encodable_num: Invalid positive integer range" invalid_pos_int_range ~print:string_of_int
      (fun i ->
        float_of_int i |> Json_JCSnafi.is_encodable_num |> not
      );

    that "unparse_jcsnafi: Invalid negative integer range" invalid_neg_int_range ~print:string_of_int
      (fun i ->
        is_exn (Invalid_argument "float or out-of-range integer")
          (fun () -> ignore (Json_JCSnafi.unparse_jcsnafi(`num (float_of_int i))))
      );
    that "unparse_jcsnafi: Invalid positive integer range" invalid_pos_int_range ~print:string_of_int
      (fun i ->
        is_exn (Invalid_argument "float or out-of-range integer")
          (fun () -> ignore (Json_JCSnafi.unparse_jcsnafi(`num (float_of_int i))))
      );    
  ] |> run_tests |> exit
