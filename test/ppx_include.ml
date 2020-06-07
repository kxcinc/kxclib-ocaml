[@@@include "ppx_include_x.ml"]

let%test "include_x" =
  _included_x = 10

module Included = struct
  [@@@include "ppx_include_y.ml"]
end

let%test "include_x" =
  Included._included_y = 20
