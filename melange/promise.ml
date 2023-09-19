(* adopted from
   https://github.com/ryyppy/rescript-promise/blob/master/src/Promise.res *)

type +'a t = 'a Js.Promise.t

exception JsError of Js.Exn.t
external unsafeToJsExn : exn -> Js.Exn.t = "%identity"

external make :
  ((('a -> unit) -> ('e -> unit) -> unit)[@mel.uncurry])
  -> 'a t
  = "Promise"
  [@@mel.new]

external resolve : 'a -> 'a t = "resolve" [@@mel.scope "Promise"]

external then_ : 'a t -> ('a -> 'b t) -> 'b t = "then" [@@mel.send]

external thenResolve : 'a t -> ('a -> 'b) -> 'b t = "then" [@@mel.send]

external finally : 'a t -> (unit -> unit) -> 'b t = "finally" [@@mel.send]

external reject : exn -> _ t = "reject" [@@mel.scope "Promise"]

external all : 'a t array -> 'a array t = "all" [@@mel.scope "Promise"]

external race : 'a t array -> 'a t = "race" [@@mel.scope "Promise"]

external _catch : 'a t -> ((exn -> 'a t)) -> 'a t = "catch" [@@mel.send]

let catch p callback =
  _catch p (fun err ->
    (* In future versions, we could use the better version:
       callback(Js.Exn.anyToExnInternal(e)) *)
    let v =
      if Js.Exn.isCamlExceptionOrOpenVariant err
      then err
      else JsError (unsafeToJsExn err) in
    callback v)
