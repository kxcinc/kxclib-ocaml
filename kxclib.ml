let inc n = n + 1
let dec n = n - 1
let refset x r = r := x
let refupdate f r = r := f !r
let refappend x r = r := x :: !r
let incr = refupdate inc
let decr = refupdate dec

let constant c = fun _ -> c
(** constant function *)

let identity x = x
(** identity function *)

let failwith' fmt =
  Format.kasprintf (failwith) fmt

let invalid_arg' fmt =
  Format.kasprintf (invalid_arg) fmt

module Functionals = struct
  let negate pred x = not (pred x) (** negate a predicate *)
  let both p g x = p x && g x
  let either p g x = p x || g x

  let dig2nd f a b = f b a         (** [f] dig the second argument of [f] to be the first. aka [flip] *)
  let dig3nd f a b c = f c a b     (** [f] dig the third argument of [f] to be the first *)
  let flip = dig2nd                (** [f] flip the first arguments of [f]. aka [dig2nd] *)
  let fix1st x f = f x             (** [x f] fix the first argument to [f] as [x] *)
  let fix2nd y f x = f x y         (** [y f] fix the second argument to [f] as [y] *)
  let fix3rd z f x y = f x y z     (** [z f] fix the third argument to [f] as [z] *)

  let tap f x =
    f x; x

  let ntimes n f x =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (f acc) (n-1) in
    loop x n

  let fixpoint ?maxn =
    match maxn with
    | None ->
       fun f x ->
       let rec loop (x,x') =
         if x = x' then x
         else loop (x', f x') in
       loop (x, f x)
    | Some 0 -> fun _ x -> x
    | Some n ->
       fun f x ->
       let rec loop n (x,x') =
         if n = 0 then x'
         else if x = x' then x
         else loop (pred n) (x', f x') in
       loop (pred n) (x, f x)
end
module Fn = Functionals

let (%) f g x = x |> g |> f
(** function composition *)

let (|&>) list f = List.map f list
(** list mapping *)

let (|!>) list f = List.iter f list
(** list iteration *)

let (&) = (@@)

let foldl = List.fold_left
(** {!List.fold_right} *)

let foldr = List.fold_right
(** {!List.fold_left} *)

(** also consider using [%debug] *)
let debug ?disabled ?label fmt = Format.(
    let disabled = Option.value ~default:false disabled in
    let ppf = err_formatter in
    let label = match label with
      | Some label -> label | None -> "unknown" in
    if disabled then ikfprintf (constant ()) ppf fmt
    else begin
        fprintf ppf "[DEBUG:%s] " label;
        kfprintf
          (fun ppf ->
            pp_print_newline ppf();
            pp_print_flush ppf ())
          ppf fmt
      end)


let projected_compare proj a b =
  compare (proj a) (proj b)

let pp_int = Format.pp_print_int
let pp_float = Format.pp_print_float
let pp_string = Format.pp_print_string
let pp_char = Format.pp_print_char
let pp_bool = Format.pp_print_bool
let pp_unit ppf () = pp_string ppf "unit"

module Either = struct
  type ('a, 'b) t = Left of 'a | Right of 'b
  let left x = Left x
  let right x = Right x
end
type ('a, 'b) either = ('a, 'b) Either.t

module Queue : sig
  type 'x t
  val empty : 'x t
  val add : 'x t -> 'x -> 'x t
  val take : 'x t -> ('x * 'x t) option
end = struct
  type 'x t = 'x list*'x list
  let empty = [], []
  let add (r,u) x = (x :: r, u)
  let rec take (r,u) = match u, r with
    | hd :: rest, _ -> Some (hd, (r, rest))
    | [], (_ :: _) -> take ([], List.rev r)
    | [], [] -> None
end
type 'x queue = 'x Queue.t

module Option = struct
  include Option

  let v default = function
    | Some x -> x
    | None -> default

  let v' gen_default = function
    | Some x -> x
    | None -> gen_default()

  let otherwise otherwise = function
    | Some x -> Some x
    | None -> otherwise

  let pp vpp ppf = Format.(function
    | Some x -> fprintf ppf "Some(%a)" vpp x
    | None -> fprintf ppf "None")
end

module Array = struct
  include Array

  (* TODO optimization - specialized version when [?f] not given *)
  let mean : ?f:('x -> float) -> float t -> float =
    fun ?f:(f=identity) arr ->
    let len = Array.length arr in
    if len = 0 then raise Not_found else
    let rec labor left right = match right - left with
      | 0 -> f arr.(left), 1
      | 1 -> (f arr.(left) +. f arr.(right) ) /. 2., 2
      | rlen ->
         if rlen < 0 then 0., 0 else
         let mid = left + (rlen / 2) in
         let lv, lw = labor left mid
         and rv, rw = labor (inc mid) right in
         let (!) = float_of_int in
         (lv *. !lw +.rv*. !rw) /. !(lw+rw), lw+rw in
    labor 0 (len-1) |> fst

  let min cmp arr = match length arr with
    | 0 -> raise Not_found
    | _ -> let cand = ref arr.(0) in
           iter (fun x -> if cmp x !cand < 0 then cand := x) arr;
           !cand

  let max cmp arr = match length arr with
    | 0 -> raise Not_found
    | _ -> let cand = ref arr.(0) in
           iter (fun x -> if cmp x !cand > 0 then cand := x) arr;
           !cand

  let first arr = match length arr with
    | 0 -> raise Not_found
    | _ -> arr.(0)

  let last arr = match length arr with
    | 0 -> raise Not_found
    | n -> arr.(n-1)

  let sorted cmp arr =
    sort cmp arr; arr

  let update : ('a -> 'a) -> 'a array -> int -> unit = fun f arr idx ->
    arr.(idx) <- f arr.(idx)
  let update_each : (int -> 'a -> 'a) -> 'a array -> unit = fun f arr ->
    arr |> Array.iteri (fun i x -> arr.(i) <- f i x)
  let blastsati : ('a -> bool) -> 'a array -> int = fun pred arr ->
    let pred i = pred arr.(i) in
    let rec loop pred l r =
      if l > r then raise Not_found
      else if l+1 = r then begin
          if pred r then r else l
        end
      else if l = r then begin
          if l = 0 && not (pred l) then raise Not_found
          else l
        end
      else let m = (l+r) / 2 in
           if pred m then loop pred m r else loop pred l (m-1)
    in loop pred 0 ((length arr) - 1)
  let blastsat : ('a -> bool) -> 'a array -> 'a = fun pred arr ->
    blastsati pred arr |> Array.get arr
  (** [blastsat] find the last element [e] such that
      [pred e] being [true] using binary search.

      more specifically,
      - when [pred] yields [false] for every element, [Not_found] is raised
      - when there exists [i >= 0] such that 
      {v     forall k <= i. (pred arr.(k)) = true
/\  forall k >  i, (pred arr.(k)) = false v}
        , the [i]-th element will be returned
      - otherwise, the behavior is undefined
   *)

  let to_function : 'a array -> (int -> 'a) =
    fun arr idx -> arr.(idx)
end

module List = struct
  include List

  let iota = function
    | 0 -> []
    | k -> 0 :: (List.init (dec k) inc)

  let range start end_exclusive = iota (end_exclusive - start) |&> (+) start

  let reduce f = function
    | [] -> raise Not_found
    | hd::tl -> foldl f hd tl

  let min cmp = function
    | [] -> raise Not_found
    | hd::l ->
       let f acc x =
         if cmp acc x > 0 then x else acc in
       fold_left f hd l

  let max cmp = function
    | [] -> raise Not_found
    | hd::l ->
       let f acc x =
         if cmp acc x < 0 then x else acc in
       fold_left f hd l

  let foldl = foldl
  let foldr = foldr

  let take n l =
    let [@warning "-8"] rec loop acc = function
      | 0,_ -> rev acc
      | n, hd::tl -> loop (hd::acc) (n-1, tl)  in
    loop [] (n, l)

  let drop n l = Fn.ntimes n tl l

  let make copies x = List.init copies (constant x)

  let count pred list =
    foldl (fun count x -> if pred x then inc count else count) 0 list
  (** [pred list] returns the number of elements [e] in [list] that satisfies [pred] *)

  let last list =
    foldl (fun _ x -> x) (List.hd list) list
  (** last element of list *)

  let fmap : ('x -> 'y list) -> 'x list -> 'y list = fun f l ->
    let rec loop acc = function
        | [] -> acc
        | x :: r -> loop ((f x |> List.rev) :: acc) r in
    let rec collect acc = function
      | [] -> acc
      | [] :: r' -> collect acc r'
      | (h :: r) :: r' -> collect (h :: acc) (r :: r')
    in
    loop [] l |> collect []

  let interpolate y xs =
    let rec loop acc = function
      | x :: [] -> x :: acc
      | [] -> acc
      | x :: xs -> loop (y :: x :: acc) xs in
    loop [] xs |> rev

  let empty = function [] -> true | _ -> false

  let to_function : 'a list -> (int -> 'a) =
    fun xs -> Array.(xs |> of_list |> to_function)

  let to_hashtbl : ('k*'v) list -> ('k, 'v) Hashtbl.t =
    fun xs -> Hashtbl.of_seq (to_seq xs)

  let pp ?sep ?parens vpp ppf xs =
    let open Format in
    let popen, pclose = match parens with
      | Some parens -> parens
      | None -> "[", "]" in
    let sep = match sep with
      | Some s -> s | None -> ";" in
    fprintf ppf "%s @[" popen;
    iter (fprintf ppf "%a%s@;" vpp |> Fn.fix2nd sep) xs;
    fprintf ppf "%s@]" pclose
end

let iota = List.iota

module Hashtbl = struct
  include Hashtbl

  let rev : ('a, 'b) t -> ('b, 'a) t = fun orig ->
    to_seq orig |> Seq.map (fun (k,v) -> (v,k)) |> of_seq
  (** swap the key and value *)

  let to_function : ('a, 'b) t -> 'a -> 'b = Hashtbl.find
end

module String = struct
  include String

  (** [chop_prefix p s] returns [s] minus the prefix [p] wrapped in [Some],
      or [None] if [s] does not start with [p] *)
  let chop_prefix prefix =
    let plen = length prefix in
    fun str ->
    let slen = length str in
    if slen < plen
    then None
    else if (sub str 0 plen) = prefix then Some (sub str plen (slen-plen))
    else None

  (** [starts_with p s] returns whether [s] starts with a substring of [p] *)
  let starts_with prefix str = chop_prefix prefix str |> Option.is_some

  (** [ends_with p s] returns whether [s] ends with a substring of [p] *)
  let ends_with postfix str =
    let plen, slen = length postfix, length str in
    if slen < plen then false
    else (sub str (slen-plen) plen) = postfix

end

module IoPervasives = struct

  let with_input_file path f =
    let ch = open_in path in
    let r =
      try f ch
      with e -> close_in ch; raise e in
    close_in ch; r

  let with_output_file path f =
    let ch = open_out path in
    let r =
      try f ch
      with e -> close_out ch; raise e in
    close_out ch; r

  let slurp_input ?buf ic =
    let buf = match buf with
      | None -> Bytes.make 4096 '\000'
      | Some buf -> buf in
    let result = ref "" in
    let rec loop len =
      match input ic buf 0 len with
      | 0 -> result
      | rlen ->
         result := !result^(Bytes.sub_string buf 0 rlen);
         loop len in
    !(loop (Bytes.length buf))

  let slurp_stdin ?buf () = slurp_input ?buf stdin

  (* optimization *)
  let slurp_file path =
    with_input_file path slurp_input

  let spit_file path str =
    with_output_file path (Fn.flip output_string str)

end include IoPervasives

module Timing = struct

  (** time the execution of [f], returning the result
        of [f] and store the measured time in [output] *)
  let timefunc' output f =
    let t = Sys.time() in
    let r = f () in
    output := Sys.time()-.t;
    r

  (** time the execution of [f], discarding the result of [f] *)
  let timefunc f =
    let time = ref 0. in
    timefunc' time f |> ignore; !time

end

module Datetime0 : sig

  (** all according to proleptic Gregorian Calender *)

  val leap_year : int -> bool
  val daycount_of_month : leap:bool -> int -> int
  val day_of_year : int -> int -> int -> int

  module type NormalizedTimestamp = sig
    (** timezone not taking into consideration *)

    module Conf : sig
      val epoch_year : int
      (** the epoch would be at January 1st 00:00:00.0 in [epoch_year] *)

      val subsecond_resolution : int
      (** e.g. sec-resolution use [1] and millisec-resolution use [1000] *)

      val min_year : int (** min-year supported *)
      val max_year : int (** max-year supported *)
    end

    val normalize : ?subsec:int ->
                    ?tzoffset:(int*int) ->
                    int*int*int ->
                    int*int*int ->
                    int
    (** [normalize
         ?tzoffset:(tzhour, tzmin)
         ?subsec (yy, mm, dd) (hour, min, sec)]
        calculates the normalized timestamp *)
  end
  module EpochNormalizedTimestamp (Conf : sig
               (** see NormalizedTimestamp *)

               val epoch_year : int
               val subsecond_resolution : int
             end) : NormalizedTimestamp

  module UnixTimestmapSecRes : NormalizedTimestamp
  module UnixTimestmapMilliRes : NormalizedTimestamp
  module UnixTimestmapNanoRes : NormalizedTimestamp

end = struct

  let sum = List.foldl (+) 0

  let days_of_months_nonleap =
    List.to_function @@
    [ 0;
      31; 28; 31; 30; 31; 30;
      31; 31; 30; 31; 30; 31; ]
  let days_of_months_leap =
    List.to_function @@
    [ 0;
      31; 29; 31; 30; 31; 30;
      31; 31; 30; 31; 30; 31; ]
  let days_of_months_subsum_nonleap =
    List.(
      iota 13
      |&> (fun x -> iota x |&> days_of_months_nonleap |> sum)
      |> to_function)
  let days_of_months_subsum_leap =
    let sum = List.foldl (+) 0 in
    List.(
      iota 13
      |&> (fun x -> iota x |&> days_of_months_leap |> sum)
      |> to_function)

  let daycount_of_month ~leap =
    let table =
      if leap
      then days_of_months_leap
      else days_of_months_nonleap in
    fun mm -> table mm

  let leap_year yy =
    let div x = yy mod x = 0 in
    if not (div 4) then false
    else if not (div 100) then true
    else if div 400 then false
    else true

  let day_of_year yy =
    let table = match leap_year yy with
      | false -> days_of_months_subsum_nonleap
      | true  -> days_of_months_subsum_leap in
    fun mm dd -> table mm + dd

  module type NormalizedTimestamp = sig
    module Conf : sig
      val epoch_year : int
      val subsecond_resolution : int
      val min_year : int
      val max_year : int
    end
    val normalize : ?subsec:int ->
                    ?tzoffset:(int*int) ->
                    int*int*int ->
                    int*int*int ->
                    int
    (** [normalize yy mm dd ?subsec hour min sec] calculates the normalized timestamp *)
  end

  (* XXX tests *)
  module EpochNormalizedTimestamp (Conf : sig
               val epoch_year : int
               val subsecond_resolution : int
             end) = struct
    module Conf = struct
      include Conf
      let min_year = Conf.epoch_year
      let max_year =
        let span =
          (pred Int.max_int)
          / (366*24*60*60*subsecond_resolution) in
        span-1+min_year
    end open Conf

    let yearcount_leaping ymin ymax =
      let roundup div = fun x ->
        if x mod div = 0 then x
        else div*(succ (x/div)) in
      let ncat div =
        let span = ymax - (roundup div ymin) in
        if span < 0 then 0
        else succ (span/div) in
      let ncat4 = ncat 4 in
      let ncat100 = ncat 100 in
      let ncat400 = ncat 400 in
      ncat4 - ncat100 + ncat400

    let normalize ?subsec ?tzoffset (yy, mm, dd) (hour, min, sec) =
      let subsec = Option.(value ~default:0 subsec) in
      if yy < min_year || yy > max_year then
        invalid_arg Format.(asprintf "%s.normalize - timestamp cannot be handled: \
                                     %d-%d-%d %02d:%02d:%02d (subsec: %d/%d) - \
                                     year out of range (%d-%d)"
                              "/kxclib.ml/.Datetime0.EpochNormalizedTimestamp"
                              yy mm dd hour min sec
                              subsec subsecond_resolution
                              min_year max_year);
      if subsec >= subsecond_resolution then
        invalid_arg Format.(sprintf "%s.normalize - subsec out of range (%d-%d)"
                              "/kxclib.ml/.Datetime0.EpochNormalizedTimestamp"
                              0 (pred subsecond_resolution));
      let days_past_years =
        let ymin, ymax = epoch_year, pred yy in
        let leaping = yearcount_leaping ymin ymax in
        let nonleaping = ymax-ymin+1-leaping in
        leaping*366+nonleaping*365 in
      let doy = day_of_year yy mm dd in
      let hour, min = match tzoffset with
        | None -> hour, min
        | Some (tzhour, tzmin) -> hour+tzhour, min+tzmin in
      let nts =
        sec + min*60 + hour*60*60
        + (days_past_years + doy)*24*60*60 in
      let nts = nts * subsecond_resolution + subsec in
      nts
  end

  module UnixTimestmapSecRes =
    EpochNormalizedTimestamp(struct
        let epoch_year = 1970
        let subsecond_resolution = 1
      end)

  module UnixTimestmapMilliRes =
    EpochNormalizedTimestamp(struct
        let epoch_year = 1970
        let subsecond_resolution = 1000
      end)

  module UnixTimestmapNanoRes =
    EpochNormalizedTimestamp(struct
        let epoch_year = 1970
        let subsecond_resolution = 1000*1000*1000
      end)

end

module ParseArgs = struct
  type optparser = string -> unit

  let prefset r x = r := x
  let prefsetv r v _ = r := v

  let scanfparser fmt fn : optparser = fun str -> Scanf.(
      sscanf str fmt fn)

  let exactparser fmt (fn : unit -> unit) : optparser = function
    | str when str = fmt -> fn ()
    | str -> raise (Invalid_argument ("exactparser not matching: "^str^":"^fmt))

  let parse_opts
        (optparsers : optparser list)
        ?argsource:(argsource=Sys.argv, 1)
        () =
    let rec tryparse str = function
      | [] -> raise (Invalid_argument ("usparsed option: "^str))
      | p::ps -> try p str with _ -> tryparse str ps in
    Array.to_list (fst argsource) |> List.drop (snd argsource)
    |!> Fn.fix2nd optparsers tryparse

  let parse_opts_args
        ?optprefix:(optprefix="-")
        ?optsep:(optsep="--")
        (optparsers : optparser list)
        ?argsource:(argsource=Sys.argv, 1)
        () =
    let source, startidx = argsource in
    let optprefixlen = String.length optprefix in
    let prefixed str =
      if String.length str < optprefixlen then false
      else (String.sub str 0 optprefixlen) = optprefix in
    let argc = Array.length source in
    let args = ref [] in
    let rec tryparse str = function
      | [] -> raise (Invalid_argument ("usparsed option: "^str))
      | p::ps -> try p str with _ -> tryparse str ps in
    let tryparse = Fn.fix2nd optparsers tryparse in
    let rec loop n parseopt =
      if n >= argc then List.rev !args
      else begin
          let arg = source.(n) in
          if not parseopt then (refappend arg args; loop (inc n) parseopt)
          else if arg = optsep then loop (inc n) false
          else if prefixed arg then (tryparse arg; loop (inc n) parseopt)
          else (refappend arg args; loop (inc n) parseopt)
        end in
    loop startidx true
end

module ArgOptions = struct

  type _ named_option =
    | IntOption : string -> int named_option
    | FloatOption : string -> float named_option
    | StringOption : string -> string named_option
    | InChannelOption : string -> in_channel named_option
    | OutChannelOption : string -> out_channel named_option
    | InChannelOption' : string -> (in_channel*channel_desc) named_option
    | OutChannelOption' : string -> (out_channel*channel_desc) named_option

  and channel_desc = [ `StandardChannel | `FileChannel of string ]

  let opt_of_named_option (type x) (opt : x named_option) = match opt with
    | IntOption opt -> opt
    | FloatOption opt -> opt
    | StringOption opt -> opt
    | InChannelOption opt -> opt
    | OutChannelOption opt -> opt
    | InChannelOption opt -> opt
    | OutChannelOption opt -> opt

  module type FeatureRequests = sig

    val has_flag :
      ?argsource:(string array*int) ->
      ?prefix:string ->
      string -> (** flag *)
      bool
    val get_option :
      ?argsource:(string array*int) ->
      ?optprefix:string ->
      ?optsep:string ->
      'x named_option ->
      'x
    val get_option_d :
      ?argsource:(string array*int) ->
      ?optprefix:string ->
      ?optsep:string ->
      'x named_option ->
      'x -> (** default value *)
      'x
    val get_option_d' :
      ?argsource:(string array*int) ->
      ?optprefix:string ->
      ?optsep:string ->
      'x named_option ->
      (unit -> 'x) -> (** default value producer *)
      'x
    val get_args :
      ?argsource:(string array*int) ->
      ?optsep:string ->
      unit -> string list
  end

  let has_flag
        ?argsource
        ?prefix:(prefix="")
        flag =
    let store = ref false in
    ParseArgs.(
      parse_opts [
          exactparser (prefix^flag) (fun () -> store := true);
          (constant ())
      ]) ?argsource ();
    !store

  let get_option
        ?argsource
        ?prefix:(prefix="")
        ?optsep
        (type x)
      : x named_option -> x option =
    let open ParseArgs in
    let labor opt f =
      let state = ref `Init in
      let result = ref None in
      let marker_raw = prefix^opt in
      let marker_eq = marker_raw^"=" in
      let par arg =
        match !state with
        | `Init when arg = marker_raw ->
           state := `CaptureNext
        | `Init ->
           (match String.chop_prefix marker_eq arg with
            | Some arg -> result := Some (f arg)
            | None -> ())
        | `CaptureNext -> (state := `Init; result := Some (f arg)) in
      parse_opts_args ?argsource ~optprefix:"" ?optsep [par] () |> ignore;
      match !state with
      | `Init -> !result
      | `CaptureNext -> invalid_arg ("no argument supplied to option "^opt) in
    function
    | IntOption opt ->
       labor opt (fun arg -> Scanf.sscanf arg "%i%!" identity)
    | FloatOption opt ->
       labor opt (fun arg -> Scanf.sscanf arg "%g%!" identity)
    | StringOption opt ->
       labor opt identity
    | InChannelOption opt ->
       labor opt (function
           | "-" -> stdin
           | path -> open_in path)
    | OutChannelOption opt ->
       labor opt (function
           | "-" -> stdout
           | path -> open_out path)
    | InChannelOption' opt ->
       labor opt (function
           | "-" -> stdin, `StandardChannel
           | path -> open_in path, `FileChannel path)
    | OutChannelOption' opt ->
       labor opt (function
           | "-" -> stdout, `StandardChannel
           | path -> open_out path, `FileChannel path)

  let get_option_exn
        ?argsource
        ?prefix
        ?optsep
        (type x)
      : x named_option -> x = fun opt ->
    match get_option ?argsource ?prefix ?optsep opt with
    | None -> invalid_arg ("you have to provide option "^(opt_of_named_option opt))
    | Some x -> x

  let get_option_d'
        ?argsource
        ?prefix
        ?optsep
        (type x)
      : x named_option -> (unit -> x) -> x = fun opt vp ->
    match get_option ?argsource ?prefix ?optsep opt with
    | None -> vp()
    | Some x -> x

  let get_option_d
        ?argsource
        ?prefix
        ?optsep
        (type x)
      : x named_option -> x -> x = fun opt v ->
    match get_option ?argsource ?prefix ?optsep opt with
    | None -> v
    | Some x -> x

  let get_absolute_args
        ?optsep:(optsep="--")
        ?argsource:(argsource=Sys.argv, 1)
        () =
    let source, startidx = argsource in
    let argc = Array.length source in
    let args = ref [] in
    let rec loop n record_arg =
      if n >= argc then List.rev !args
      else begin
          let arg = source.(n) in
          if record_arg then (refappend arg args; loop (succ n) record_arg)
          else if arg = optsep then loop (succ n) true
          else loop (succ n) record_arg
        end in
    loop startidx false

end
