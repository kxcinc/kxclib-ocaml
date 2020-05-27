let strhas subs str =
  Str.(string_match
         (regexp (Format.sprintf ".*%s.*"
                    (quote subs)))
         str 0)

let%test "noimplval" =
  try ([%noimplval]; false)[@warning "-21"]
  with Failure _ -> true

let%test "noimplval:str" =
  try ([%noimplval "yes"]; false)[@warning "-21"]
  with Failure msg ->
    strhas "noimpl" msg && strhas "yes" msg

let%test "noimplfun" =
  try ([%noimplfun] (); false)[@warning "-21"]
  with Failure _ -> true

let%test "noimplfun:str" =
  try ([%noimplfun "yes"] (); false)[@warning "-21"]
  with Failure msg ->
    strhas "noimpl" msg && strhas "yes" msg
