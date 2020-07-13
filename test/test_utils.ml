open Kxclib

let%test _ = List.iota 0 = []
let%test _ = List.iota 1 = [0]
let%test _ = List.iota 3 = [0; 1; 2]
