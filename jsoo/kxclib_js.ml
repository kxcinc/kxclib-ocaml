open Kxclib

type js_val = Js_of_ocaml.Js.Unsafe.any

module Json_ext : sig
  open Kxclib.Json

  type xjv = js_val
  (** external json value, simply a JavaScript value *)

  val to_xjv : jv -> xjv
  val of_xjv : xjv -> jv

  val to_json_string : jv -> string
  val of_json_string_opt : string -> jv option
end = struct
  external _cast : 'a -> 'b = "%identity"
  open Js_of_ocaml
  type any = js_val

  type jv = Kxclib.Json.jv
  let jstr : string -> any = Js.string &> _cast
  let ocstr : 'x. 'x -> string = fun x -> _cast x |> Js.to_string
  let _call = Js.Unsafe.meth_call
  let _is_null : any -> bool = fun x -> x == (Js.null |> _cast)
  let _is_undefined : any -> bool = fun x -> x == (Js.undefined |> _cast)

  type xjv = js_val
  type xjv_type = [
    | `_undefined
    | `_null
    | `_boolean
    | `_number
    | `_bigint
    | `_string
    | `_symbol
    | `_function
    | `_object
    | `_array
    ]

  let _js_array : any = Js_of_ocaml.Js.Unsafe.pure_js_expr "Array"
  let _js_object : any = Js_of_ocaml.Js.Unsafe.pure_js_expr "Object"
  let _js_json : any = Js_of_ocaml.Js.Unsafe.pure_js_expr "JSON"

  let _jstr_length : any = jstr "length"

  let _to_jstr : 'x -> any =
    fun v ->
    if _is_null v then jstr "null" else
    if _is_undefined v then jstr "undefined" else
    jstr @@ _call v "toString" [||]

  let classify : xjv -> xjv_type =
    let _undefined = jstr "undefined" in
    let _boolean = jstr "boolean" in
    let _number = jstr "number" in
    let _bigint = jstr "bigint" in
    let _string = jstr "string" in
    let _symbol = jstr "symbol" in
    let _function = jstr "function" in
    let _object = jstr "object" in
    fun x ->
    let typ : any = Js.typeof x |> _cast in
    if typ == _string then `_string
    else if typ == _undefined then `_undefined
    else if typ == _boolean then `_boolean
    else if typ == _number then `_number
    else if typ == _bigint then `_bigint
    else if typ == _symbol then `_symbol
    else if typ == _function then `_function
    else if typ == _object then (
      if _is_null x then `_null
      else if _call _js_array "isArray" [| x |] |> Js.to_bool
      then `_array
      else `_object)
    else invalid_arg' "unable to classify object %s" (
             _to_jstr x |> ocstr)

  let rec to_xjv : jv -> xjv = function
    | `null -> Js.null |> _cast
    | `bool x -> Js.bool x |> _cast
    | `num x -> x |> _cast
    | `str x -> jstr x
    | `arr arr ->
       let arr = Array.of_list arr in
       let v = Js.Unsafe.new_obj _js_array [| Array.length arr |> _cast |] |> _cast in
       arr |> Array.iteri (fun idx x -> Js.array_set v idx (to_xjv x));
       v |> _cast
    | `obj kvs ->
       Js.Unsafe.obj [||] |-> (fun o ->
        kvs |!> (fun (key, value)
                 -> Js.Unsafe.set o
                      (jstr key)
                      (to_xjv value)))

  let rec of_xjv : xjv -> jv = fun x ->
    match classify x with
    | `_null -> `null
    | `_boolean -> `bool (_cast x |> Js.to_bool)
    | `_number -> `num (_cast x)
    | `_string -> `str (_cast x |> Js.to_string)
    | `_array ->
       let len : int = Js.Unsafe.get x _jstr_length |> _cast in
       let v = x |> _cast in
       let xs =
         Kxclib.Array.init len (fun idx ->
             Js.array_get v idx |> _cast |> of_xjv
           ) in
       `arr (xs |> Array.to_list)
    | `_object ->
       let es = _call _js_object "entries" [| x |] in
       let len : int = Js.Unsafe.get es _jstr_length |> _cast in
       let rec loop acc = function
         | (-1) -> acc
         | n ->
            let entry = Js.Unsafe.get es n in
            let field_value = Js.Unsafe.get entry 1 in
            let acc =
              if _is_undefined field_value then acc
              else
                let field =
                  Js.Unsafe.get entry 0 |> ocstr,
                  field_value |> _cast |> of_xjv
                in
                (field :: acc)
            in
            loop acc (pred n) in
       `obj (loop [] (pred len))
    | _ -> invalid_arg' "of_xjv: unable to convert non-JSON encodable value %s" (
               _to_jstr x |> ocstr)

  let to_json_string = to_xjv &> Json.output &> Js.to_string
  let of_json_string_opt str =
    try
      _call _js_json "parse" [| Js.string str |> _cast |]
      |> of_xjv |> some
    with Js_error.Exn _ ->
      None
end

module Promise' = Prr.Jv.Promise

module Promise = struct
  open Promise'
  type nonrec _ t = t
end

module Promise_io : sig
  include Io_style with type 'a t = 'a Promise.t
end = struct
  type 'x t = 'x Promise.t

  let return x : _ t = Promise'.resolve x

  let bind : 'x t -> ('x -> 'y t) -> 'y t =
    fun m af ->
    let then_ p res =
      Prr.Jv.(call p "then" [| callback ~arity:1 res;|])
    in
    then_ m af

  let inject_error (e: exn) : _ t =
    let e = Prr.Jv.(Error.v (Printexc.to_string e |> to_jstr % of_string)) in
    Promise'.reject e

  let inject_error' ((e, _): exn * backtrace_info option): 'x t =
    let e = Prr.Jv.(Error.v (Printexc.to_string e |> to_jstr % of_string)) in
    Promise'.reject e

  let extract_error : 'x t -> ('x, exn * backtrace_info option) result t =
    fun m ->
    let _cast = Obj.magic in
    Promise'.then' m (fun x -> Result.ok x |> return) (fun exn ->
      let stack =
        let open Prr.Jv in
        (exn |> to_option _cast)
        >? (fun e -> get (Obj.magic e) "stack")
        >>? to_option (to_string) in
      let bt : backtrace_info option =
        match stack with
        | Some stack -> `string_stacktrace stack |> some
        | None -> none in
      Result.error (exn, bt) |> return)

  let trace (s: string) = Prr.Console.(log [ Prr.Jv.of_string s ]); return ()
end
