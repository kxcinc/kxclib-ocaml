let inc n = n + 1
let dec n = n - 1
let refset x r = r := x
let refupdate f r = r := f !r
let incr = refupdate inc
let decr = refupdate dec

let constant c = fun _ -> c
(** constant function *)

let identity x = x
(** identity function *)

module Functionals = struct
  let negate pred x = not (pred x) (** negate a predicate *)
  let both p g x = p x && g x
  let either p g x = p x || g x

  let dig2nd f a b = f b a         (** [f] dig the second argument of [f] to be the first. aka [flip] *)
  let dig3nd f a b c = f c a b     (** [f] dig the third argument of [f] to be the first *)
  let flip = dig2nd                (** [f] flip the first arguments of [f]. aka [dig2nd] *)
  let fix1st x f = f x             (** [x f] fix the first argument to [f] as [x] *)
  let fix2nd y f x = f x y         (** [y f] fix the second argument to [f] as [y] *)
  let fix3nd z f x y = f x y z     (** [z f] fix the third argument to [f] as [z] *)

  let ntimes n f x =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (f acc) (n-1) in
    loop x n
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
let debug ?label fmt = Format.(
    let ppf = err_formatter in
    let label = match label with
      | Some label -> label | None -> "unknown" in
    fprintf ppf "[DEBUG:%s] " label;
    kfprintf (fun ppf ->
               pp_print_newline ppf();
               pp_print_flush ppf ())
      ppf fmt)

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

  let otherwise otherwise = function
    | Some x -> Some x
    | None -> otherwise

  let pp vpp ppf = Format.(function
    | Some x -> fprintf ppf "Some(%a)" vpp x
    | None -> fprintf ppf "None")
end

module Array = struct
  include Array
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
    let rec loop acc = function
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

  let pp vpp ppf xs =
    let open Format in
    fprintf ppf "[ @[";
    iter (fprintf ppf "%a;@;" vpp) xs;
    fprintf ppf "]@]";
end

module Hashtbl = struct
  include Hashtbl

  let rev : ('a, 'b) t -> ('b, 'a) t = fun orig ->
    to_seq orig |> Seq.map (fun (k,v) -> (v,k)) |> of_seq
  (** swap the key and value *)

  let to_function : ('a, 'b) t -> 'a -> 'b = Hashtbl.find
end

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
    timefunc' time f; !time

end
