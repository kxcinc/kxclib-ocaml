let esc s =
  "\"" ^ String.escaped s
  ^ ( if String.escaped s <> s
      then "\"mlesc" else "\"")

let rec show_jv : Json.jv -> string = function
  (* | `null -> "null" *)
  (* | `bool true -> "true" *)
  (* | `bool false -> "false" *)
  | (`null | `bool _ | `num _) as j -> Json.show j
  | `str s -> esc s
  | `arr xs -> "[" ^ (String.concat","(xs |&> show_jv)) ^ "]"
  | `obj fs -> "[" ^ (String.concat","(fs |&> fun (k, v) -> esc k ^ ":" ^ show_jv v)) ^ "]"
