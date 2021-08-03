open Sexplib.Std

type device_descr = {
    fd : Unix.file_descr;
    blocking : bool;
  }

exception No_enough_input_available

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

let packet_reader st0 func =
  { pr_initst = st0;
    pr_func = !!func }

module PrivateUnix = struct
  include Unix


  let file_perm_of_sexp = int_of_sexp
  let sexp_of_file_perm = sexp_of_int
  let write_string fd str =
    let len = String.length str in
    match write_substring fd str 0 len with
    | n when n = len -> ()
    | _ -> failwith "panic"
  let read_string fd n
        ?buf:(buf=Bytes.create n)
        ?auto_retry:(retry=false)
        ?retry_delay:(delay=0.000010 (* 10us *) )
        ()
        =
    let rec loop readn =
      try match read fd buf readn (n - readn) with
          | m when m + readn = n -> Bytes.sub_string buf 0 n
          | m when m + readn < n && m > 0 ->
             if retry then (
               Unix.sleepf delay;
               loop (m + readn)
             ) else raise No_enough_input_available
          | _ -> failwith "panic"
      with Unix_error (Unix.EAGAIN, _, _) as e ->
        if retry then (
          Unix.sleepf delay;
          loop readn
        ) else raise e in
    loop 0

  let read_string fd
        ?buf
        ?auto_retry ?retry_delay
        n = read_string fd n ?buf ?auto_retry ?retry_delay ()
end

let read_string dev = PrivateUnix.read_string dev.fd
let write_string = PrivateUnix.write_string
let read_single ?buf ?auto_retry ?retry_delay fd =
  String.get (read_string fd ?buf ?auto_retry ?retry_delay 1) 0

type device_attr = Unix.terminal_io = {
    mutable c_ignbrk : bool;
    mutable c_brkint : bool;
    mutable c_ignpar : bool;
    mutable c_parmrk : bool;
    mutable c_inpck : bool;
    mutable c_istrip : bool;
    mutable c_inlcr : bool;
    mutable c_igncr : bool;
    mutable c_icrnl : bool;
    mutable c_ixon : bool;
    mutable c_ixoff : bool;
    mutable c_opost : bool;
    mutable c_obaud : PrivateUnix.file_perm;
    mutable c_ibaud : PrivateUnix.file_perm;
    mutable c_csize : PrivateUnix.file_perm;
    mutable c_cstopb : PrivateUnix.file_perm;
    mutable c_cread : bool;
    mutable c_parenb : bool;
    mutable c_parodd : bool;
    mutable c_hupcl : bool;
    mutable c_clocal : bool;
    mutable c_isig : bool;
    mutable c_icanon : bool;
    mutable c_noflsh : bool;
    mutable c_echo : bool;
    mutable c_echoe : bool;
    mutable c_echok : bool;
    mutable c_echonl : bool;
    mutable c_vintr : char;
    mutable c_vquit : char;
    mutable c_verase : char;
    mutable c_vkill : char;
    mutable c_veof : char;
    mutable c_veol : char;
    mutable c_vmin : PrivateUnix.file_perm;
    mutable c_vtime : PrivateUnix.file_perm;
    mutable c_vstart : char;
    mutable c_vstop : char;
  } [@@deriving sexp]

let pp_device_attr ~hum ppf attr =
  let pp_sexp = Sexplib.Sexp.(if hum then pp_hum else pp_mach) in
  Format.(fprintf ppf "%a" pp_sexp (sexp_of_device_attr attr))

module PrivateHelpers = struct
  let default_umask = 0o22
end open PrivateHelpers

(* ref: https://stackoverflow.com/a/26006680/15838152
 *      - good resource regarding blocking/non-blocking and vmin/vtime *)
let make_device_attr
      ?parity_mark:(parmark=false)
      ?parity_bit:(parbit=false)
      ?parity_odd:(parodd=false)

      ?stopbit_count:(stopbit=1)
      ?char_size:(cs=8)

      ?baud:(baud=9600)

      ?read_min:(vmin=1)      (* unit: number of chars *)
      ?read_timeout:(vtime=0) (* unit: tenth of seconds *)

      () = Unix.{
      c_ignbrk = false;
      c_brkint = false;
      c_ignpar = false;
      c_parmrk = parmark;
      c_inpck = parmark;
      c_istrip = false;
      c_inlcr = false;
      c_igncr = false;
      c_icrnl = false;
      c_ixon = false;
      c_ixoff = false;
      c_opost = false;
      c_obaud = baud;
      c_ibaud = baud;
      c_csize = cs;
      c_cstopb = stopbit;
      c_cread = true;
      c_parenb = parbit;
      c_parodd = parodd;
      c_hupcl = false;
      c_clocal = true;
      c_isig = false;
      c_icanon = false;
      c_noflsh = false;
      c_echo = false;
      c_echoe = false;
      c_echok = false;
      c_echonl = false;
      c_vintr = '\003';
      c_vquit = '\028';
      c_verase = '\127';
      c_vkill = '\021';
      c_veof = '\004';
      c_veol = '\255';
      c_vmin = vmin;
      c_vtime = vtime;
      c_vstart = '\017';
      c_vstop = '\019';
           }

let get_device_attr dev =
  Unix.(tcgetattr dev.fd)

let set_device_attr dev
      ?setattr_when:(t=Unix.TCSANOW)
      attr =
  Unix.(tcsetattr dev.fd t attr)

let open_device
      ?non_blocking:(nb=false)
      ?baud ?device_attr:(attr=make_device_attr ?baud ())
      path =
    let flags = Unix.[ O_RDWR; O_NOCTTY ] in
    let flags = match nb with
      | false -> flags
      | true -> O_NONBLOCK :: flags in
    let fd = Unix.(openfile path flags default_umask) in
    Unix.(tcsetattr fd TCSANOW attr);
    { fd; blocking = not nb }

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
end = struct
  open Lwt.Infix
  let ret = Lwt.return

  type raw_device_descr = device_descr

  type device_descr = Lwt_unix.file_descr

  let of_raw_device_descr dev =
    Lwt_unix.of_unix_file_descr dev.fd
      ~blocking:dev.blocking
      ~set_flags:false

  let write_string fd str =
    let len = String.length str in
    Lwt_unix.write_string fd str 0 len >>= function
    | n when n = len -> ret ()
    | _ ->
       let stats = Unix.fstat (Lwt_unix.unix_file_descr fd) in
       failwith' "panic @Serial_unix.Lwt.write_string fd[%d:%d] \"%s\""
             Unix.(stats.st_dev) Unix.(stats.st_ino)
             str

  let read_string0 fd n
        ?buf:(buf=Bytes.create n)
        ?auto_retry:(retry=true)
        ?retry_delay:(delay=0.000010 (* 10us *) ) =
    let rec loop readn =
      Lwt_unix.read fd buf readn (n - readn)(* read fd buf readn *) >>= function
      | 0 -> failwith' "panic @Serial_unix.Lwt.read: EOF reached"
      | m when m+readn = n -> Bytes.sub_string buf 0 n |> ret
      | m when m+readn < n && m > 0 ->
         if retry then (
           Lwt_unix.sleep delay >>= fun() ->
           loop (m+readn)
         ) else raise No_enough_input_available
      | m -> failwith' "panic @Serial_unix.Lwt.read: m=%d" m
    in loop 0

  let read_string fd ?buf ?auto_retry ?retry_delay n = read_string0 ?buf ?auto_retry ?retry_delay fd n

  let read_single ?buf ?auto_retry ?retry_delay fd =
    read_string fd ?buf ?auto_retry ?retry_delay 1
    >|= Fn.flip String.get 0

  let read_packet fd ?buf ?auto_retry ?retry_delay reader =
    let buf = lazy (Option.v' (fun() -> Bytes.create 512) buf) in
    let buf() = Lazy.force buf in
    let func = reader.pr_func in
    let rec loop st = function
      | `Bulk n ->
         read_string fd ~buf:(buf()) ?auto_retry ?retry_delay n >>= fun str ->
         func (st, `BulkRead str) |> proc
      | `Single ->
         read_single fd ~buf:(buf()) ?auto_retry ?retry_delay >>= fun ch ->
         func (st, `CharRead ch) |> proc
    and proc = function
      | `ReadBulk (st, n) -> loop st (`Bulk n)
      | `ReadChar st -> loop st `Single
      | `Pause (st, delay, next) ->
         Lwt_unix.sleep delay >>= fun() ->
         proc next
      | `Finish x -> ret x
    in func (reader.pr_initst, `Init) |> proc
end

