type device_attr = Unix.terminal_io
(** use [make_device_attr] to generate one *)

type device_descr

module Dangerous : sig val file_descr_of_device_descr : device_descr -> Unix.file_descr end

type ('st, 'ret) packet_reader = {
    pr_initst : 'st;
    pr_func :
      'st*[
      | `Init
      | `CharRead of char
      | `BulkRead of string ]
      -> ([
      | `ReadBulk of 'st*int | `ReadChar of 'st
      | `Pause of 'st*float*'c (* sec *)
      | `Finish of 'ret ] as 'c)
  }

exception No_enough_input_available

val packet_reader :
  'st ->
  ('st ->
   [ `BulkRead of string | `CharRead of char | `Init ] ->
   ([ `Finish of 'ret
    | `Pause of 'st * float * 'act
    | `ReadBulk of 'st * int
    | `ReadChar of 'st ] as 'act)) ->
  ('st, 'ret) packet_reader

val open_device :
  ?non_blocking:bool ->
  ?baud:int -> ?device_attr:device_attr -> string
  -> device_descr

val write_string : Unix.file_descr -> string -> unit
(** [write_string fd str] writes [str] to [fd] *)

val read_string :
  device_descr ->
  ?buf:bytes ->
  ?auto_retry:bool -> ?retry_delay:float
  -> int -> string
(** [read_string fd ?buf ?auto_retry ?retry_delay n]
    reads [n] bytes from [fd]. If [buf] is not specified, an
    internal buffer will be created. If specified, it must be more than [n] bytes.
    You can specify [~auto_retry:true] to ... *)

val read_single :
  ?buf:bytes ->
  ?auto_retry:bool -> ?retry_delay:float -> device_descr
  -> char
(** [read_single fd ?buf ?auto_retry ?retry_delay n] ... *)

val pp_device_attr : hum:bool -> Format.formatter -> device_attr -> unit

val make_device_attr :
  ?parity_mark:bool ->
  ?parity_bit:bool ->
  ?parity_odd:bool ->
  ?stopbit_count:int ->
  ?char_size:int ->
  ?baud:int -> ?read_min:int -> ?read_timeout:int -> unit -> device_attr

val get_device_attr : device_descr -> device_attr

val set_device_attr :
  device_descr -> ?setattr_when:Unix.setattr_when
  -> device_attr -> unit

module Lwt : sig
  type raw_device_descr = device_descr
  type device_descr

  val of_raw_device_descr : raw_device_descr -> device_descr

  val write_string : device_descr -> string -> unit Lwt.t
  val read_string :
    device_descr
    -> ?buf:bytes -> ?auto_retry:bool -> ?retry_delay:float -> int
    -> string Lwt.t
  val read_single :
    ?buf:bytes -> ?auto_retry:bool -> ?retry_delay:float
    -> device_descr -> char Lwt.t
  val read_packet :
    device_descr
    -> ?buf:bytes -> ?auto_retry:bool -> ?retry_delay:float
    -> ('a, 'b) packet_reader -> 'b Lwt.t
end
