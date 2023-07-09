(* adopted from
   https://github.com/ryyppy/rescript-promise/blob/master/src/Promise.res *)

type +'a t = 'a Js.Promise.t

exception JsError of Js.Exn.t
external unsafeToJsExn : exn -> Js.Exn.t = "%identity"

external make :
  ((('a -> unit) -> ('e -> unit) -> unit)[@bs.uncurry])
  -> 'a t
  = "Promise"
  [@@bs.new]

external resolve : 'a -> 'a t = "resolve" [@@bs.val] [@@bs.scope "Promise"]

external then_ : 'a t -> ('a -> 'b t) -> 'b t = "then" [@@bs.send]

external thenResolve : 'a t -> ('a -> 'b) -> 'b t = "then" [@@bs.send]

external finally : 'a t -> (unit -> unit) -> 'b t = "finally" [@@bs.send]

external reject : exn -> _ t = "reject" [@@bs.val] [@@bs.scope "Promise"]

external all : 'a t array -> 'a array t = "all" [@@bs.val] [@@bs.scope "Promise"]

external race : 'a t array -> 'a t = "race" [@@bs.val] [@@bs.scope "Promise"]

external _catch : 'a t -> ((exn -> 'a t)) -> 'a t = "catch" [@@bs.send]

let catch p callback =
  _catch p (fun err ->
    (* In future versions, we could use the better version:
       callback(Js.Exn.anyToExnInternal(e)) *)
    let v =
      if Js.Exn.isCamlExceptionOrOpenVariant err
      then err
      else JsError (unsafeToJsExn err) in
    callback v)
