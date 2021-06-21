let () =
  let open Kxclib_fastpng in
  let w, h = 9, 12 in
  let ba = create_rgba_buffer w h in
  for i = 0 to (pred w) do
    for j = 0 to (pred h) do
      let c = if i = j
              then Int32.(of_int 0xffccff |> logor (of_int 0x5f |> Fn.flip shift_left (8*3)))
              else match  [@warning "-8"] j mod 3 with
                   | 0 -> pixel32_of_rgba 0 0 0xff 0xff
                   | 1 -> pixel32_of_rgba 0 0xff 0 0x8f
                   | 2 -> pixel32_of_rgba 0xff 0 0 0xff
      in
      set_pixel32 ba i j c
    done
  done;
  let filename = "test-fastpng.png" in
  Format.printf "size = (%d, %d) ; output to : %s\n" w h filename;
  write_png_rgba ~filename ba
