[%%import "lib_versions.mlh"]

[%%if YOJSON_VERSION < (3, 0, 0)]
type yojson = ([
    | `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `String of string
    | `Assoc of (string * 't) list
    | `List of 't list
    | `Tuple of 't list
    | `Variant of string * 't option
    ] as 't)
[%%else]
type yojson = ([
    | `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `String of string
    | `Assoc of (string * 't) list
    | `List of 't list
    ] as 't)
[%%endif]
