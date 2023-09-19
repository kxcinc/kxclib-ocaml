module Json_ext : sig
  type xjv
  (** external json value, simply a JavaScript value *)

  val to_xjv : Json.jv -> xjv
  val of_xjv : xjv -> Json.jv

  val to_json_string : Json.jv -> string
  val of_json_string_opt : string -> Json.jv option
end = struct
  external _cast : 'a -> 'b = "%identity"
  let _stringify : 'a. 'a -> string = fun x -> [%bs.raw {| function(x){return ''+x;} |}] (_cast x)

  type xjv

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