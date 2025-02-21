open Kxclib
open Kxclib_js

open struct
  external _cast : 'a -> 'b = "%identity"
end

module Jest = struct
  type t
  external expect : 'a -> t = "expect" [@@mel.module "bun:test"]
  external to_be : t -> 'a -> unit = "toBe" [@@mel.send]
  external to_strict_equal : t -> 'a -> unit = "toStrictEqual" [@@mel.send]
  external to_be_undefined : t -> unit = "toBeUndefined" [@@mel.send]
end

open Jest

module Json_ext_test : sig
  val test_with_samples : unit -> unit
  val test_of_json_string_opt_failure : unit -> unit
  val test_string_success : unit -> unit
end = struct
  type test_sample = {
    jv: Json.jv;
    json: Js.Json.t; }
  
  let samples: test_sample array = [|
    { jv = `null;
      json = [%mel.raw {| null |}] };
    { jv = `bool true;
      json = [%mel.raw {| true |}] };
    { jv = `bool false;
      json = [%mel.raw {| false |}] };
    { jv = `num 131.;
      json = [%mel.raw {| 131 |}] };
    { jv = `num 131.338;
      json = [%mel.raw {| 131.338 |}] };
    { jv = `str "hello?";
      json = [%mel.raw {| "hello?" |}] };
    { jv = `str "\x58";
      json = [%mel.raw {| "\x58" |}] };
    { jv = `str "日本語";
      json = Js.Json.string "日本語" };
    { jv = `arr [];
      json = [%mel.raw {| [] |}] };
    { jv = `obj [ "", `obj [] ];
      json = [%mel.raw {| {"": {}} |}] };
    { jv = `arr [ `arr [] ];
      json = [%mel.raw {| [ [] ] |}] };
    { jv = `arr [ `num 1.; `str "(a number)" ];
      json = [%mel.raw {| [ 1, "(a number)" ] |}] };
    { jv = `obj [ "best", `arr []; "friend", `num 23. ];
      json = [%mel.raw {| { best: [], friend: 23 } |}] };
    { jv = `arr [ `bool true; `str "hello?" ];
      json = [%mel.raw {| [ true, "hello?" ] |}] }
  |]
  
  let test_with_samples () =
    samples |> Array.iter (fun { jv; json } ->
      let xjv = Json_ext.to_xjv jv in
      to_strict_equal (expect xjv) json;
      
      let jv_of_xjv = Json_ext.of_xjv xjv in
      to_strict_equal (expect jv_of_xjv) jv;
      
      let str = Json_ext.to_json_string(jv) in
      let jv_of_str = Json_ext.of_json_string_opt str in
      to_strict_equal (expect jv_of_str) jv
    )
    
  let test_of_json_string_opt_failure () =
    Json_ext.of_json_string_opt "{" |> expect |> to_be_undefined;
    
    (* violating JSON spec as field name not properly quoted *)
    Json_ext.of_json_string_opt "{age: 12}" |> expect |> to_be_undefined;
    
    (* violating JSON spec as field name not properly quoted *)
    Json_ext.of_json_string_opt "{'age': 12}" |> expect |> to_be_undefined
    
  let test_string_success () =
    samples |> Array.iter (fun { jv; json } ->
      let parsed01 = json |> Js.Json.stringify |> Json_ext.of_json_string_opt |> Option.get in
      to_strict_equal (expect (Json_ext.to_xjv parsed01)) json;
      
      let parsed02 = jv |> Json_ext.to_json_string |> Js.Json.parseExn in
      to_strict_equal (expect parsed02) json
    )
end

module Promise_io_test : sig
  val tests : (unit -> unit Promise_io.t) array
end = struct
  let test01 () =
    Promise_io.(bind (return 1) @@ fun x ->
      to_be (expect (x + 1)) 2;
      return ())

  let test02 () =
    let open Promise_io in
    let error = Failure "error test" in
    inject_error error
    |> extract_error
    |> Fn.flip bind @@ function
      | Ok () -> failwith "unexpected"
      | Error (exn, _backtrace) ->
        to_strict_equal (expect exn) error;
        return ()
        
  let test03 () =
    let module M = MonadOps(Promise_io) in
    let open M in
    return ()
    
  let test04 () =
    let module M = MonadOps(Promise_io) in
    let open M in
    return 40 >>= fun x ->
      to_be (expect (x + 2)) 42;
      return ()

  let tests = [|
    Promise_io.return;
    test01;
    test02;
    test03;
    test04;
  |]
end
