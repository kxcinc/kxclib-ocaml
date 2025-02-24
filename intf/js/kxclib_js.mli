open Kxclib

(* a JavaScript value *)
type js_val

module Json_ext : sig

  (** [Json_ext] concerns the type [xjv], meaning external json value.

      It only makes sense on a virtual machine shared with a JavaScript runtime. *)

  type xjv = private js_val

  val to_xjv : Json.jv -> xjv
  val of_xjv : xjv -> Json.jv

  val to_json_string : Json.jv -> string
  val of_json_string_opt : string -> Json.jv option
end

module Promise : sig
  type 'x t

  (* TODO: *)
  (* val create : (('a -> unit) -> ('b -> unit) -> unit) -> t *)
  (* val resolve : 'a -> t *)
  (* val reject : 'a -> t *)
  (* val await : t -> ('a -> unit) -> unit *)
  (* val bind : t -> ('a -> t) -> t *)
  (* val then' : t -> ('a -> t) -> ('b -> t) -> t *)
  (* val all : t list -> t *)
end

module Promise_io : sig
  include Io_style with type 'a t = 'a Promise.t
end

(* TODO: *)
(* module Futexn_io = Futexn_io *)

