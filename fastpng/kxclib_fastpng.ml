open Bigarray

type pixel32 = Int32.t
type pixel32_buffer = (int32, int32_elt, c_layout) Bigarray.Array2.t

(** pixel32_buffer should contain int32 data of 0xaarrggbb *)
external _fastpng_libpng_write_rgba : string -> pixel32_buffer -> bool -> unit = "fastpng_libpng_write_rgba"

let create_rgba_buffer w h : pixel32_buffer =
  Array2.create int32 c_layout w h

let pixel32_of_rgba r g b a =
  Int32.(
    let (<<) = shift_left in
    let (!) = of_int in
    let (|/) = logor in
       (!a << (8*3)) |/ (!r << (8*2))
    |/ (!g << (8*1)) |/ (!b << (8*0)))

let rgba_of_pixel32 p =
  Int32.(
    let (>>) = shift_right_logical in
    let (!) = of_int in
    let (/|) = logand in
    let mask = (Int32.of_int 0xff) in
    (p >> (8*3)) /| mask,
    (p >> (8*2)) /| mask,
    (p >> (8*1)) /| mask,
    (p >> (8*0)) /| mask)

let set_pixel32 buff x y p = Array2.set buff x y p
let get_pixel32 buff x y = Array2.get buff x y

let set_rgba buff x y r g b a =
  Array2.set buff x y (pixel32_of_rgba r g b a)
let get_rgba buff x y = get_pixel32 buff x y |> rgba_of_pixel32

let write_png_rgba :
      ?fast_compression:bool
      -> filename:string
      -> pixel32_buffer -> unit =
  fun ?fast_compression:(fc=false) ~filename ba ->
  _fastpng_libpng_write_rgba filename ba fc
