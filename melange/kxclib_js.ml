open Kxclib

type js_val

module Promise' = Kxclib_melange_promise.Promise

module Promise : sig
  type 'x t = 'x Promise'.t
end = Promise'

module Promise_io : sig
  include Io_style with type 'a t = 'a Promise.t
end = struct
  type 'x t = 'x Promise.t

  let return x : _ t = Promise'.resolve x

  let bind : 'x t -> ('x -> 'y t) -> 'y t = fun m af -> Js.Promise.then_ af m

  let inject_error (e: exn) : _ t = Promise'.reject e

  let inject_error' ((e, _): exn * backtrace_info option): 'x t =
    Js.Promise.reject e

  let extract_error : 'x t -> ('x, exn * backtrace_info option) result t =
    fun m ->
    let m = bind m (fun x -> Result.ok x |> return) in
    Promise'.catch m (fun exn ->
      let stack = Js.Exn.asJsExn exn >>? Js.Exn.stack in
      let bt : backtrace_info option =
        match stack with
        | Some stack -> `string_stacktrace stack |> some
        | None -> none in
      Result.error (exn, bt) |> return)

  let trace (s: string) = Js.Console.log(s); return ()
end

module Json_ext : sig
  type xjv  = private js_val
  (** external json value, simply a JavaScript value *)

  val to_xjv : Json.jv -> xjv
  val of_xjv : xjv -> Json.jv

  val to_json_string : Json.jv -> string
  val of_json_string_opt : string -> Json.jv option
end = struct
  external _cast : 'a -> 'b = "%identity"
  let _stringify : 'a. 'a -> string =
    fun x -> [%mel.raw {| function(x){return ''+x;} |}]
               (_cast x) [@@warning "-20"]

  type xjv = js_val

  let to_xjv : Json.jv -> xjv =
    let rec to_json = function
      | `null -> Js.Json.null
      | `bool x -> Js.Json.boolean x
      | `num x -> Js.Json.number x
      | `str x -> Js.Json.string x
      | `arr arr -> arr |. Belt.List.toArray |. Belt.Array.map to_json |> Js.Json.array
      | `obj kvs ->
        Js.Dict.empty()
        |-> (fun o ->
          kvs |. Belt.List.forEach (fun (key, value) ->
                     Js.Dict.set o key (to_json value)))
        |> Js.Json.object_
    in to_json &> _cast

  let rec of_xjv : xjv -> Json.jv = fun x ->
    match Js.Types.classify x with
    | JSUndefined -> invalid_arg "of_xjv: 'undefined' not expected"
    | JSFunction f -> invalid_arg ("of_xjv: function not expected: "^(_stringify f))
    | JSSymbol symb -> invalid_arg ("of_xjv: symbol not expected: "^(_stringify symb))
    | JSBigInt x -> invalid_arg ("of_xjv: bigint not expected: "^(_stringify x))
    | JSNull -> `null
    | JSFalse -> `bool false
    | JSTrue -> `bool true
    | JSNumber x -> `num x
    | JSString x -> `str x
    | JSObject obj ->
      if Js.Array.isArray obj then (
        `arr (_cast obj |. Belt.Array.map of_xjv |> Belt.List.fromArray)
      ) else (
        let fs = Belt.Array.makeUninitialized 0 |> _cast in
        Js.Dict.entries (_cast obj)
        |. Belt.Array.forEach (fun (k, v) ->
               if not Js.Types.(test v Undefined)
               then Belt.Array.push fs (k, of_xjv v));
        `obj (fs |> Belt.List.fromArray)
      )

  let to_json_string: Json.jv -> string = to_xjv &> _cast &> Js.Json.stringify
  let of_json_string_opt: string -> Json.jv option =
    fun str ->
    try Js.Json.parseExn str |> _cast |> of_xjv |> some
    with _ -> None
end
