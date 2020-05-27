open Kxclib

let%test _ = iota 0 = []
let%test _ = iota 1 = [0]
let%test _ = iota 3 = [0; 1; 2]
