open Prr
open Kxclib

open struct
  let cast = Obj.magic
  let jstr x = Jstr.v x |> cast

  module F = struct
    open Prr.Fut
    let return0 = return
    let ( >>=! ) = bind
  end

  type backtrace = backtrace_info [@@deriving show]
end

open F
type 'x fut = 'x Prr.Fut.t

let pp_fut pp_x ppf (fut : _ fut) = Fut.await fut (pp_x ppf)

let current_backtrace () : backtrace =
  match Sys.backend_type with
  | Native | Bytecode -> `ocaml_backtrace (Printexc.get_raw_backtrace ())
  | Other backend ->
    print_endline ("other_backend: " ^ backend);
    let stacktrace =
      Js_of_ocaml.(
        (Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "Error") [||])##.stack
        |> Js.to_string) in
    `string_stacktrace stacktrace

type 'x t = ('x, exn * backtrace) result fut [@@deriving show]

let await : 'a t -> (('a, exn * backtrace) result -> unit) -> unit = Fut.await

let return : 'x. 'x -> 'x t = fun x -> Result.ok x |> return0
let inject_error' : 'x. exn * backtrace_info option -> 'x t =
  fun (e, bt) ->
  Log0.debug "Io.inject_error: %s" (Printexc.to_string e);
  let bt = bt |?! current_backtrace in
  Result.error (e, bt) |> return0
let inject_error : 'x. exn -> 'x t = fun e -> inject_error' (e, None)

let trace : string -> unit t =
  fun s ->
  Log0.log ~label:"trace" ~header_style:(Some `Thin) ~header_color:`Yellow
    "%s" s;
  return ()

let bind : 'x 'y. 'x t -> ('x -> 'y t) -> 'y t =
  fun m af ->
  m >>=! function
  | Error e -> Error e |> return0
  | Ok x -> ( try af x with e -> inject_error e)

let extract_error : 'x t -> ('x, exn * backtrace_info option) result t =
  fun m ->
  m >>=! function
  | Ok x -> Result.ok x |> return
  | Error (e, bt) -> Result.error (e, some bt) |> return

let tick_ms ms = Fut.tick ~ms |> Fut.map Result.ok

let to_promise : error:(exn * backtrace -> Prr.Jv.t) -> Prr.Jv.t t -> Prr.Jv.Promise.t =
  fun ~error m -> Fut.to_promise' m ~ok:identity ~error

let wrap_js_result : (Prr.Jv.t, Prr.Jv.Error.t) result t -> Prr.Jv.t t =
  fun m ->
  let ( >>= ) m f = bind m f in
  m >>= function
  | Ok v -> return v
  | Error je ->
    let module Err = Prr.Jv.Error in
    Error
      ( Failure (Err.message je |> Jstr.to_string),
        `string_stacktrace (Err.stack je |> Jstr.to_string) )
    |> return0

let wrap_future_result : ('a, Prr.Jv.Error.t) result fut -> 'a t =
  fun m ->
  let ( >>= ) m f = Fut.bind m f in
  m >>= function
  | Ok v -> return v
  | Error je ->
    let module Err = Prr.Jv.Error in
    Error
      ( Failure (Err.message je |> Jstr.to_string),
        `string_stacktrace (Err.stack je |> Jstr.to_string) )
    |> return0

let of_promise : 'a =
  fun ?err_message js_promise ->
  Fut.of_promise' js_promise ~ok:identity ~error:(fun err ->
    let msg = Prr.Console.str err |> Jstr.to_string in
    (match err_message with
    | None -> msg
    | Some m -> m ^ ": " ^ msg)
    |> jstr |> Prr.Jv.Error.v)
  |> wrap_future_result
