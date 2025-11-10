open Kxclib_js

let fold_promise = List.foldl Promise_io.bind (Promise_io.return ())

module Jest = struct
  type t
  external expect : 'a -> t = "expect" [@@mel.module "bun:test"]
  external to_be : t -> 'a -> unit = "toBe" [@@mel.send]
  external to_strict_equal : t -> 'a -> unit = "toStrictEqual" [@@mel.send]
  external to_be_undefined : t -> unit = "toBeUndefined" [@@mel.send]
  external to_throw : t -> string -> unit = "toThrow" [@@mel.send]
end
