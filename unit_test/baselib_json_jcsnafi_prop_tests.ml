let min_fi_float = Float.of_int (- (1 lsl 52)) ;;
let max_fi_float = (Float.of_int ((1 lsl 52) - 1)) ;;
let min_fi_int = (- (1 lsl 52)) ;;
let max_fi_int = (1 lsl 52) - 1 ;;

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

  let invalid_integer_unparse_jcsnafi_prop name min max = that name (QCheck2.Gen.int_range min max) ~print:string_of_int
      (fun i -> is_exn (Invalid_argument "float or out-of-range integer")
           (fun () -> ignore (Json_JCSnafi.unparse_jcsnafi(`num (float_of_int i))))) in

  let run_tests tests = QCheck_base_runner.run_tests_main tests in
  [
    invalid_integer_unparse_jcsnafi_prop "unparse_jcsnafi: Invalid negative integer range" min_int (pred min_fi_int);
    invalid_integer_unparse_jcsnafi_prop "unparse_jcsnafi: Invalid positive integer range" (succ max_fi_int) max_int;
  ] |> run_tests |> exit
