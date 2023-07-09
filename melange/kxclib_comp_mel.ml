module Kxclib_comp_mel = struct

module BaseCompInternals = struct
  type _jsobj
  let _flip f a b = f b a
  external _cast : 'a -> 'b = "%identity"
  let _obj_make() : _jsobj = Js.Obj.empty() |> _cast
end open BaseCompInternals

module Ppx_deriving_runtime = struct
  module Format = Stdlib.Format
  type nonrec int = int
  type nonrec char = char
  type nonrec string = string
  type nonrec float = float
  type nonrec bool = bool
  type nonrec unit = unit
  type nonrec exn = exn
  type nonrec 'a array = 'a array
  type nonrec 'a list = 'a list
  type nonrec 'a option = 'a option
  type nonrec nativeint = nativeint
  type nonrec int32 = int32
  type nonrec int64 = int64
  type nonrec 'a lazy_t = 'a lazy_t
  type nonrec bytes = bytes
end

module Float = struct
  module Imported = struct
    external cast : 'a -> 'b = "%identity"
    external _isInteger : float -> bool = "isInteger"
    [@@bs.val][@@bs.scope "Number"]
  end open Imported

  let ceil = Js.Math.ceil_float
  let floor = Js.Math.floor_float
  let is_integer = _isInteger
  let to_int : float -> int = fun x -> _cast x

  let max = max
  let min = min
  let equal = (==)
  let compare a b = if a = b then 0 else if a > b then 1 else (-1)

  let neg x = (-.x)
  let add a b = a +. b
  let sub a b = a -. b
  let mul a b = a *. b
  let div a b = a /. b
  let rem a b = Int.rem (cast a) (cast b) |> cast
  let succ x = x +. 1.
  let pred x = x -. 1.
  let abs = abs_float

  let one = 1.
  let zero = 0.
  let minus_one = (-1.)

  let to_string = Js.Float.toString
  let of_int = float_of_int
end

module Bytes = struct
include Bytes
external unsafe_set_uint8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"
external set_int8 : bytes -> int -> int -> unit = "%bytes_safe_set"
external get_uint8 : bytes -> int -> int = "%bytes_safe_get"
end

module Buffer = struct
include Buffer

(* adopted from ocaml 4.12.0 source tree *)

let add_utf_8_uchar b u = match Uchar.to_int u with
  | u when u < 0 -> assert false
  | u when u <= 0x007F ->
     add_char b (Char.unsafe_chr u)
  | u when u <= 0x07FF ->
     let buf = Bytes.create 2 in
     Bytes.unsafe_set buf (0    )
       (Char.unsafe_chr (0xC0 lor (u lsr 6)));
     Bytes.unsafe_set buf (0 + 1)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     add_bytes b buf
  | u when u <= 0xFFFF ->
     let buf = Bytes.create 3 in
     Bytes.unsafe_set buf (0    )
       (Char.unsafe_chr (0xE0 lor (u lsr 12)));
     Bytes.unsafe_set buf (0 + 1)
       (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
     Bytes.unsafe_set buf (0 + 2)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     add_bytes b buf
  | u when u <= 0x10FFFF ->
     let buf = Bytes.create 4 in
     Bytes.unsafe_set buf (0    )
       (Char.unsafe_chr (0xF0 lor (u lsr 18)));
     Bytes.unsafe_set buf (0 + 1)
       (Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F)));
     Bytes.unsafe_set buf (0 + 2)
       (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
     Bytes.unsafe_set buf (0 + 3)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     add_bytes b buf
  | _ -> assert false

let add_int8 =
  let buf = lazy (Bytes.create 1) in
  fun b x ->
  let buf = Lazy.force buf in
  Bytes.set_int8 buf 0 x;
  add_bytes b buf

let add_uint8 =
  let buf = lazy (Bytes.create 1) in
  fun b x ->
  let buf = Lazy.force buf in
  Bytes.unsafe_set_uint8 buf 0 x;
  add_bytes b buf
end
end


module Kxclib_comp = struct
  (* perhaps due to a bug of ReScript compiler, simply using a module alias would produce
     an undefined variable Kxclib_comp in the compiled js file *)
  include Kxclib_comp_mel
end

module Buffer = Stdlib.Buffer
