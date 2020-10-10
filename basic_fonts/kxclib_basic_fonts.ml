module type BitmapFont1 = sig
  val font_name      : string
  val font_license   : string option
  val glyph_dimension : int*int (** pixels in width*height *)
  val glyph_exists   : char -> bool
  val glyph_table    : char -> (int*int) array
end

type block_measurement =
  { lefttop     : int*int;
    rightbottom : int*int;
    width  : int;
    height : int; }

module type DrawingUtils = sig

  val draw_char :
    ?scale:int ->
    dotfunc:(int -> int -> unit) ->
    int -> int
    -> char -> unit

  val draw_char_measured :
    ?scale:int ->
    dotfunc:(int -> int -> unit) ->
    int -> int
    -> char -> block_measurement

  val draw_string :
    ?scale:int ->
    dotfunc:(int -> int -> unit) ->
    int -> int
    -> string -> unit
    
  val draw_string_measured :
    ?scale:int ->
    dotfunc:(int -> int -> unit) ->
    int -> int
    -> string -> block_measurement
end

module MakeDrawingUtils1(Font : BitmapFont1) : DrawingUtils
  = struct
  open Font

  let draw_char_measured ?scale:(scale=1) ~dotfunc x y c =
    let char = glyph_table c in
    char |> Array.iter (fun (x', y') ->
                for x' = (x'*scale) to (x'*scale)+scale-1 do
                  for y' = (y'*scale) to (y'*scale)+scale-1 do
                    dotfunc (x+x') (y+y')
                  done
                done);
    let cw, ch = glyph_dimension in
    let width, height = cw*scale, ch*scale in
    { lefttop = (x, y);
      rightbottom = (x+cw*scale, y+ch*scale);
      width; height; }

  let draw_char ?scale ~dotfunc x y c =
    draw_char_measured ?scale ~dotfunc x y c |> ignore

  let draw_string_measured ?scale:(scale=1) ~dotfunc x y str =
    let cw, ch = glyph_dimension in
    let width = cw*scale in
    let offset = ref x in
    let shift() = let x = !offset in offset := x + width; x in
    str |> String.iter (fun c -> draw_char ~scale ~dotfunc (x+shift()) y c |> ignore);
    let strlen = String.length str in
    { lefttop = (x, y);
      rightbottom = (x+cw*scale*strlen, y+cw*scale);
      width = cw*scale*strlen; height = cw*scale}

  let draw_string ?scale ~dotfunc x y str =
    draw_string_measured ?scale ~dotfunc x y str |> ignore
  
end


