module Json_ext : sig
  type xjv
  (** external json value, simply a JavaScript value *)
  
  val to_xjv : Json.jv -> xjv
  val of_xjv : xjv -> Json.jv

  val to_json_string : Json.jv -> string
  val of_json_string_opt : string -> Json.jv option
end = struct
  external _cast : 'a -> 'b = "%identity"
  external _stringify : 'a -> string = "JSON.stringify" [@@bs.val]

  type xjv
  
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
    
  let to_xjv : Json.jv -> xjv =
    let rec to_json = function
      | `null -> Js.Json.null
      | `bool x -> Js.Json.boolean x
      | `num x -> Js.Json.number x
      | `str x -> Js.Json.string x
      | `arr arr -> arr |> Array.of_list |> Array.map to_json |> Js.Json.array
      | `obj kvs ->
        Js.Dict.empty()
        |-> (fun o ->
          kvs |!> (fun (key, value) -> Js.Dict.set o key (to_json value)))
        |> Js.Json.object_
    in to_json &> _cast

  let rec of_xjv : xjv -> Json.jv = fun x ->
    match Js.Types.classify x with
    | JSUndefined -> invalid_arg "of_xjv: 'undefined' not expected"
    | JSFunction f -> invalid_arg ("of_xjv: function not expected: "^(_stringify f))
    | JSSymbol symb -> invalid_arg ("of_xjv: function not expected: "^(_stringify symb))
    | JSNull -> `null
    | JSFalse -> `bool false
    | JSTrue -> `bool true
    | JSNumber x -> `num x
    | JSString x -> `str x
    | JSObject obj ->
      if Js.Array.isArray obj then (
        `arr (_cast obj |> Array.map of_xjv |> Array.to_list)
      ) else (
        Js.Dict.entries (_cast obj)
        |> Array.map (fun (k, v) -> (k, of_xjv v))
        |> fun xs -> `obj (Array.to_list xs)
      )
      
  let to_json_string: Json.jv -> string = to_xjv &> _stringify
  let of_json_string_opt: string -> Json.jv option =
    fun str ->
    try Js.Json.parseExn str |> _cast |> of_xjv |> some
    with _ -> None
end