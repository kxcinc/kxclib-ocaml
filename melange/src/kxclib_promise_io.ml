module PromiseIo : sig
  include Io_style with type 'a t = 'a Promise.t
end = struct
  type 'x t = 'x Promise.t

  let return x : _ t = Promise.resolve x

  let bind : 'x t -> ('x -> 'y t) -> 'y t = fun m af -> Js_promise.then_ af m

  let inject_error (e: exn) : _ t = Js_promise.reject e

  let inject_error' ((e, _): exn * backtrace_info option): 'x t =
    Js_promise.reject e

  let extract_error : 'x t -> ('x, exn * backtrace_info option) result t =
    fun m ->
    let m = bind m (fun x -> Result.ok x |> return) in
    Promise.catch m (fun exn ->
      let stack = Js_exn.asJsExn exn >>? Js_exn.stack in
      let bt : backtrace_info option =
        match stack with
        | Some stack -> `string_stacktrace stack |> some
        | None -> none in
      Result.error (exn, bt) |> return)

  let trace (s: string) = Js.Console.log(s); return ()
end
