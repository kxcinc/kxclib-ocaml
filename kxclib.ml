let refset r x = r := x
(** [refset r x] sets [x] to ref [r]. *)

let refupdate r f = r := f !r
(** [refupdate r f] updates referent of [r] by [f]. *)

let refappend r x = r := x :: !r
(** [refappend r x] appends [x] to referent of [r]. *)

let refupdate' f r = r := f !r
(** [refupdate' f r] is equivalent to [refupdate r f]. *)

let refappend' x r = r := x :: !r
(** [refappend' x r] is equivalent to [refappend r x]. *)

let refpop r = match !r with h::t -> r:=t; h | [] -> raise Not_found
(** [refpop r] pop first item of the list referred to by [r].
    {b Raises} [Not_found] if the list is empty. *)

let incr = refupdate' succ
(** [incr r] increases the referent of [r] by one. *)

let decr = refupdate' pred
(** [decr r] decreases the referent of [r] by one. *)

let refupdate'_and_get f r = r := f !r; !r
let get_and_refupdate' f r = let x = !r in r := f !r; x

let incr_and_get = refupdate'_and_get succ
let decr_and_get = refupdate'_and_get pred

let get_and_incr = get_and_refupdate' succ
let get_and_decr = get_and_refupdate' pred

let constant c = fun _ -> c
(** constant function *)

let identity x = x
(** identity function *)

let failwith' fmt =
  Format.kasprintf (failwith) fmt

let invalid_arg' fmt =
  Format.kasprintf (invalid_arg) fmt

let iotaf n func =
  let rec loop acc = function
    | m when m = n -> acc
    | m -> loop (func m :: acc) (succ m) in
  loop [] 0 |> List.rev

let iotaf' n func =
  let rec loop = function
    | m when m = n -> ()
    | m -> func m; loop (succ m) in
  loop 0

let iotafl n func acc0 =
  let rec loop acc = function
    | m when m = n -> acc
    | m -> loop (func acc m) (succ m) in
  loop acc0 0

let min_by f x y = if f y > f x then x else y
let max_by f x y = if f y < f x then x else y

type exn = Printexc.t

module Functionals = struct
  let negate pred x = not (pred x)
  (** negate a predicate *)

  let both p g x = p x && g x
  let either p g x = p x || g x

  let dig2nd f a b = f b a
  (** [f] dig the second argument of [f] to be the first. aka [flip] *)

  let dig3rd f a b c = f c a b
  (** [f] dig the third argument of [f] to be the first *)

  let flip = dig2nd
  (** [f] flip the first arguments of [f]. aka [dig2nd] *)

  let fix1st x f = f x
  (** [x f] fix the first argument to [f] as [x] *)

  let fix2nd y f x = f x y
  (** [y f] fix the second argument to [f] as [y] *)

  let fix3rd z f x y = f x y z
  (** [z f] fix the third argument to [f] as [z] *)

  let fix1st' x f = fun _ -> f x
  (** [x f] fix the first argument to [f] as [x], but still accept (and ignore) the fixed argument *)

  let tap f x =
    f x; x

  let reptill judge f x =
    let rec loop y =
      if judge y then y
      else loop (f x) in
    loop (f x)
  (** [reptill judge f x] evaluates [f x] repeatedly till [judge (f x)] holds. *)

  let ntimes n f x =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (f acc) (n-1) in
    loop x n
  (** [ntimes n f x] applies [f] ntimes to [x]. *)

  let dotill judge f x =
    let rec loop y =
      if judge y then y
      else loop (f y) in
    loop (f x)
  (** [dotill judge f x] applies [f] to [x] repeatedly till [judge x] holds. *)

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
  (** [fixpoint f] try to resolve the fixpoint of f.
      [maxn], an optional argument, limits the number of iterations
      to find the fix point. *)

  let converge' judge f =
    let rec loop n (x, x') =
      match judge n x x' with
      | true  -> Ok x'
      | false -> loop (succ n) (x', f x') in
    fun x -> loop 1 (x, f x)

  let converge judge f x =
    converge' (fun _ x x' -> judge x x') f x

  module BasicInfix = struct
    (** function composition 1 *)
    let (%) : ('y -> 'z) -> ('x -> 'y) -> ('x -> 'z) =
      fun f g x -> x |> g |> f

    (** function composition 1, on second argument *)
    let (%%) : ('a -> 'y -> 'z) -> ('x -> 'y) -> ('a -> 'x -> 'z) =
      fun f g x y -> f x (g y)

    (** function composition 2 *)
    let (&>) : ('x -> 'y) -> ('y -> 'z) -> ('x -> 'z) =
      fun g f x -> x |> g |> f

    (** function composition 2, arity=2 *)
    let (&&>) : ('x -> 'y -> 'z) -> ('z -> 'r) -> ('x -> 'y -> 'r) =
      fun g f x y -> g x y |> f

    (** piping with tapping *)
    let (|->) : 'x -> ('x -> unit) -> 'x = fun x f -> (f x); x

    let (//) : ('a -> 'x) -> ('b -> 'y) -> ('a*'b -> 'x*'y) =
      fun fa fb (a, b) -> fa a, fb b

    let (/>) : 'a*'b -> ('b -> 'c) -> 'a*'c =
      fun (a, b) f -> a, f b

    let (/<) : 'a*'b -> ('a -> 'c) -> 'c*'b =
      fun (a, b) f -> f a, b

    (** lift to snd *)
    let (?>) : ('b -> 'c) -> ('a*'b -> 'a*'c) =
      fun f -> fun (a, b) -> a, f b

    (** lift to fst *)
    let (?<) : ('a -> 'c) -> ('a*'b -> 'c*'b) =
      fun f -> fun (a, b) -> f a, b

    (** uncurry *)
    let (!!) : ('a -> 'b -> 'x) -> ('a*'b -> 'x) =
      fun f -> fun (a, b) -> f a b

    (** curry *)
    let (!?) : ('a*'b -> 'x) -> ('a -> 'b -> 'x) =
      fun f -> fun a b -> f (a, b)
  end

  module CommonTypes = struct
    type 'x endo = 'x -> 'x
  end

  module Infix = BasicInfix
end
module Fn = Functionals
include Functionals.BasicInfix
include Functionals.CommonTypes

module PipeOps(S : sig
             type _ t
             val map : ('x -> 'y) -> 'x t -> 'y t
             val iter : ('x -> unit) -> 'x t -> unit
             val fold_left : ('acc -> 'x -> 'acc) -> 'acc -> 'x t -> 'acc
             val filter : ('x -> bool) -> 'x t -> 'x t
             val filter_map : ('x -> 'y option) -> 'x t -> 'y t
           end) = struct
  open S

  (** piping map *)
  let (|&>) : 'x t -> ('x -> 'y) -> 'y t = fun xs f -> map f xs

  (** piping map to snd *)
  let (|+&>) : 'x t -> ('x -> 'y) -> ('x*'y) t =
    fun xs f -> map (fun x -> x, f x) xs

  (** piping iter *)
  let (|!>) : 'x t -> ('x -> unit) -> unit =
    fun xs f -> iter f xs

  (** piping and iter-tapping *)
  let (|-!>) : 'x t -> ('x -> unit) -> 'x t =
    fun xs f -> iter f xs; xs

  (** piping fold_left *)
  let (|@>) : 'x t -> ('acc*('acc*'x -> 'acc)) -> 'acc =
    fun xs (z, f) -> fold_left (fun acc x -> f (acc, x)) z xs

  (** piping filter *)
  let (|?>) : 'x t -> ('x -> bool) -> 'x t = fun xs f -> filter f xs

  (** piping filter map *)
  let (|&?>) : 'x t -> ('x -> 'y option) -> 'y t = fun xs f -> filter_map f xs

  (** piping filter map to snd *)
  let (|+&?>) : 'x t -> ('x -> 'y option) -> ('x*'y) t =
    fun xs f -> filter_map (fun x -> match f x with Some y -> Some (x, y) | None -> None) xs
end

module type Monadic = sig
  type _ t
  val return : 'x -> 'x t
  val bind : 'x t -> ('x -> 'y t) -> 'y t
end

module MonadOps(M : sig
             type _ t
             val return : 'x -> 'x t
             val bind : 'x t -> ('x -> 'y t) -> 'y t
           end) = struct
  let return x = M.return x
  let (>>=) = M.bind
  let (>>) : 'x M.t -> 'y M.t -> 'y M.t =
    fun ma mb -> ma >>= fun _ -> mb
  let (>|=) : 'x M.t -> ('x -> 'y) -> 'y M.t =
    fun ma f -> ma >>= fun x -> return (f x)

  let sequence_list ms =
    List.fold_left (fun acc m ->
        acc >>= fun acc ->
        m >>= fun x ->
        x :: acc |> return
      ) (return []) ms >>= fun xs ->
    List.rev xs |> return

  let (>>=*) : 'x M.t list -> ('x list -> 'y M.t) -> 'y M.t =
    fun ms af -> sequence_list ms >>= af
end

let foldl = List.fold_left
(** {!List.fold_right} *)

let foldr f z l = List.fold_right f l z
(** {!List.fold_left} but arg pos exchanged *)

let projected_compare proj a b =
  compare (proj a) (proj b)

module Either = struct
  type ('a, 'b) t = Left of 'a | Right of 'b
  let left x = Left x
  let right x = Right x
end
type ('a, 'b) either = ('a, 'b) Either.t

module Result = struct
  include Result

  (** NB - returning only the first error *)
  let concat : ('x, 'e) result list -> ('x list, 'e) result =
    fun rs ->
    let rec loop acc = function
      | [] -> Ok acc
      | Ok x :: rest -> loop (x :: acc) rest
      | Error e :: _ -> Error e in
    loop [] (List.rev rs)
end

module ResultOf(E : sig type err end) = struct
  type err = E.err
  type 'x t = ('x, err) result
  let bind : 'x t -> ('x -> 'y t) -> 'y t = Result.bind
  let return : 'x -> 'x t = Result.ok
end

module ResultWithErrmsg = struct
  include ResultOf(struct type err = string end)
  let protect' : handler:(exn -> string) -> ('x -> 'y) -> ('x -> 'y t) =
    fun ~handler f x ->
    try Ok (f x)
    with exn -> Error (handler exn)
  let protect : ('x -> 'y) -> ('x -> 'y t) =
    fun f -> protect' ~handler:Printexc.to_string f
end

module Queue : sig
  type 'x t
  val empty : 'x t
  val is_empty : 'x t -> bool
  val push : 'x -> 'x t -> 'x t
  val push_front : 'x -> 'x t -> 'x t
  val pop : 'x t -> ('x * 'x t) option
  val peek : 'x t -> ('x * 'x t) option
end = struct
  type 'x t = 'x list*'x list
  let empty = [], []
  let is_empty = function
    | [], [] -> true
    | _ -> false
  let push x (r,u) = (x :: r, u)
  let push_front x (r,u) = (r, x :: u)
  let rec pop (r,u) = match u, r with
    | hd :: rest, _ -> Some (hd, (r, rest))
    | [], (_ :: _) -> pop ([], List.rev r)
    | [], [] -> None
  let rec peek (r,u as q) = match u, r with
    | hd :: _, _ -> Some (hd, q)
    | [], (_ :: _) -> peek ([], List.rev r)
    | [], [] -> None
end
type 'x queue = 'x Queue.t

module Option = struct
  include Option

  let return = some

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

  let filter pred = function
    | Some x when pred x -> Some x
    | _ -> None

  let fmap f = function
    | None -> None
    | Some v -> (
      match f v with
      | None -> None
      | Some v -> v)

  let of_bool = function
    | true -> Some ()
    | false -> None

  let some_if cond x = if cond then Some x else None
end
let some = Option.some
let none = Option.none
let (>?) o f = Option.map f o
let (>>?) o f = Option.bind o f
let (|?) o v = Option.v v o
let (||?) o1 o2 = Option.otherwise o2 o1

module Seq = struct
  include Seq
  include PipeOps(Seq)

  let from : (unit -> 'x option) -> 'x t =
    fun f ->
    let rec next() = match f() with
    | None -> Nil
    | Some x -> Cons (x, next) in
    next

  let iota until_exclusive =
    let counter = ref 0 in
    from (fun() ->
        let x = !counter in
        if x = until_exclusive
        then None
        else (
          incr counter;
          Some x
        )
      )

  let length s =
    fold_left (fun c _ -> succ c) 0 s

  let range ?include_endpoint:(ie=false) start end_ =
    let end_exclusive = if ie then succ end_ else end_ in
    iota (end_exclusive - start) |&> (+) start

  let enum start =
    let counter = ref start in
    from (fun () ->
        get_and_incr counter |> Option.some
      )

  let rec limited quota orig() =
    if quota > 0 then (
      let open Seq in
      match orig() with
      | Nil -> Nil
      | Cons (x, next) ->
         Cons (x, limited (pred quota) next)
    ) else Nil

  let iteri f s =
    let rec h i = function
      | Nil -> ()
      | Cons(x, rest) -> f i x; h (i + 1) (rest()) in
    s() |> h 0

  let hd s =
    match s() with
    | Nil -> raise Not_found
    | Cons(x, _) -> x

  let tl s =
    match s() with
    | Nil -> raise Not_found
    | Cons(_, t) -> t

  let take n s =
    match n with
    | _ when n < 0 -> failwith "panic"
    | _ ->
       let rec h n t () =
         match n, (t()) with
         | 0, _ -> Nil
         | _, Nil -> failwith "panic"
         | _, Cons(x, u) ->
            Cons(x, h (n - 1) u) in
       h n s

  let drop n s = Fn.ntimes n tl s

  let make n x =
    match n with
    | _ when n < 0 -> failwith "panic"
    | _ ->
       let rec h i () =
         match i with
         | 0 -> Nil
         | _ -> Cons(x, h (i - 1)) in
       h n

end
type 'x seq = 'x Seq.t

module Array = struct
  include Array

  let filter f arr =
    arr |> to_seq |> Seq.filter f |> of_seq
  let filter_map f arr =
    arr |> to_seq |> Seq.filter_map f |> of_seq

  include PipeOps(struct
              include Array
              let filter = filter
              let filter_map = filter_map
            end)

  let of_list_of_length len list =
    let cell = ref list in
    init len (fun _ ->
        match !cell with
        | hd :: tl ->
           cell := tl;
           hd
        | [] -> raise Not_found)

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
         and rv, rw = labor (succ mid) right in
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

  let swap arr idx1 idx2 =
    let tmp = arr.(idx2) in
    arr.(idx2) <- arr.(idx1);
    arr.(idx1) <- tmp

  let shuffle : ?rng:(int (* bound_exclusive *) -> int) -> 'x array -> unit =
    fun ?rng:(rng=Random.int) arr ->
    let len = Array.length arr in
    for i = len-1 downto 1 do
      swap arr i (rng (succ i))
    done

  let to_function : 'a array -> (int -> 'a) =
    fun arr idx -> arr.(idx)
end

module Stream = struct
  include Stream

  let to_list_rev stream =
    let result = ref [] in
    Stream.iter (fun value -> result := value :: !result) stream;
    !result

  let to_list stream =
    to_list_rev stream |> List.rev

  let hd stream =
    let open Stream in
    try next stream with
    | Failure -> raise Not_found

  let drop1 stream =
    let open Stream in
    let _ = try next stream with
    | Failure -> raise Not_found in
    stream

  let take n stream =
    let open Stream in
    let m_lst =
      try npeek n stream with
      | Failure -> raise Not_found
      | Error msg -> failwith msg in
    match List.length m_lst with
    | m when m = n -> m_lst
    | _ -> raise Not_found

  let drop n s = Fn.ntimes n drop1 s
end

module List = struct
  include PipeOps(List)
  include List

  let iota = function
    | 0 -> []
    | k -> 0 :: (List.init (pred k) succ)

  let range =
    let helper start end_ = iota (end_ - start) |&> (+) start in
    fun ?include_endpoint:(ie=false) ->
    if ie then (fun start end_ -> helper start (succ end_))
    else (fun start end_ -> helper start end_)

  let dedup l =
    let set = Hashtbl.create (List.length l) in
    l |?> (fun x ->
      if Hashtbl.mem set x then false
      else (Hashtbl.add set x true; true))

  let update_assoc : 'k -> ('v option -> 'v option) -> ('k*'v) list -> ('k*'v) list
    = fun k func l ->
    let l', updated =
      l |> fold_left (fun (acc, updated) (key, v as ent) ->
               match updated, k = key with
               | false, true -> (
                 match func (some v) with
                 | Some v' -> (key, v') :: acc, true
                 | None -> acc, true)
               | _ -> ent :: acc, updated
             ) ([], false) in
    if not updated then (
      match func none with
      | None -> l
      | Some v -> (k, v) :: l
    ) else rev l'

  let update_assq : 'k -> ('v option -> 'v option) -> ('k*'v) list -> ('k*'v) list
    = fun k func l ->
    let l', updated =
      l |> fold_left (fun (acc, updated) (key, v as ent) ->
               match updated, k == key with
               | false, true -> (
                 match func (some v) with
                 | Some v' -> (key, v') :: acc, true
                 | None -> acc, true)
               | _ -> ent :: acc, updated
             ) ([], false) in
    if not updated then (
      match func none with
      | None -> l
      | Some v -> (k, v) :: l
    ) else rev l'

  let group_by : ('x -> 'k) -> 'x t -> ('k*'x t) t =
    fun kf l ->
    l |> fold_left (fun acc x ->
             let k = kf x in
             update_assoc k (function
                 | Some xs -> x :: xs |> some
                 | None -> some [x]) acc)
           []

  let unzip l =
    List.fold_left
      (fun (l,s) (x,y) -> (x::l,y::s))
      ([],[]) (List.rev l)

  let unzip3 l =
    List.fold_left
      (fun (l1,l2,l3) (x1,x2,x3) -> (x1::l1,x2::l2,x3::l3))
      ([],[],[]) (List.rev l)

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

  let hd = function
    | [] -> raise Not_found
    | h :: _ -> h

  let tl = function
    | [] -> raise Not_found
    | _ :: tail -> tail

  let take n l =
    let rec loop acc = function
      | 0, _ -> rev acc
      | n, hd::tl -> loop (hd::acc) (n-1, tl)
      | _ -> raise Not_found in
    loop [] (n, l)

  let drop n l = Fn.ntimes n tl l

  let make copies x = List.init copies (constant x)

  let count pred list =
    foldl (fun count x -> if pred x then succ count else count) 0 list
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

  let filteri p l =
    let rec aux i acc = function
      | [] -> rev acc
      | x::l -> aux (i + 1) (if p i x then x::acc else acc) l
    in
    aux 0 [] l

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

  let bind ma af = fmap af ma
  let return x = [x]
end

include PipeOps(List)

let iota = List.iota

module Hashtbl = struct
  include Hashtbl

  let rev : ('a, 'b) t -> ('b, 'a) t = fun orig ->
    to_seq orig |> Seq.map (fun (k,v) -> (v,k)) |> of_seq
  (** swap the key and value *)

  let to_function : ('a, 'b) t -> 'a -> 'b = Hashtbl.find

  (** [make n genfunc] creates a hashtable of [n] elements with entries
      [{ (fst (genfunc 0))  |-> (snd (genfunc 0))
       , (fst (genfunc 1))  |-> (snd (genfunc 1))
         ...
       , (fst (genfunc (n-1)))  |-> (snd (genfunc (n-1)))
       }]  *)
  let make ?random : int -> (int -> 'a * 'b) -> ('a, 'b) Hashtbl.t =
    fun n genfunc ->
    let table = Hashtbl.create ?random n in
    Seq.iota n |> Seq.map genfunc |> Hashtbl.add_seq table;
    table

end

module String = struct
  include String


  (** [empty str] returns true when str is of zero length *)
  let empty str = length str = 0

  (** [empty_trimmed str] returns true when str is of zero length after being trimmed *)
  let empty_trimmed str = length (trim str) = 0

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

  (** [chop_prefix p s] returns [s] minus the suffix [p] wrapped in [Some],
      or [None] if [s] does not end with [p] *)
  let chop_suffix suffix =
    let plen = length suffix in
    fun str ->
    let slen = length str in
    if slen < plen then None
    else if (sub str (slen-plen) plen) = suffix then (
      Some (sub str 0 (slen-plen))
    ) else None

  let to_list str =
    to_seq str |> List.of_seq

  let of_list = List.to_seq &> of_seq
  let of_array = Array.to_seq &> of_seq
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

      val min_year : int
      (** min-year supported *)

      val max_year : int
      (** max-year supported *)

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
    else if div 400 then true
    else false

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
  type optparser = string -> [`Process_next of bool]

  let prefset r x = r := x
  let prefsetv r v _ = r := v

  let scanfparser fmt fn : optparser = fun str ->
    Scanf.ksscanf str (fun _ _ -> `Process_next true) fmt fn;
    `Process_next false

  let exactparser fmt (fn : unit -> unit) : optparser = function
    | str when str = fmt -> fn (); `Process_next false
    | _ -> `Process_next true

  let parse_opts
        (optparsers : optparser list)
        ?argsource:(argsource=Sys.argv, 1)
        () =
    let rec tryparse str = function
      | [] -> raise (Invalid_argument ("unparsed option: "^str))
      | p::ps ->
         match (p : optparser) str with
         | `Process_next true -> tryparse str ps
         | `Process_next false -> () in
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
      | [] -> raise (Invalid_argument ("unparsed option: "^str))
      | p::ps ->
         match p str with
         | `Process_next true -> tryparse str ps
         | `Process_next false -> () in
    let tryparse = Fn.fix2nd optparsers tryparse in
    let rec loop n parseopt =
      if n >= argc then List.rev !args
      else begin
          let arg = source.(n) in
          if not parseopt then (refappend args arg; loop (succ n) parseopt)
          else if arg = optsep then loop (succ n) false
          else if prefixed arg then (tryparse arg; loop (succ n) parseopt)
          else (refappend args arg; loop (succ n) parseopt)
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
    | InChannelOption' opt -> opt
    | OutChannelOption' opt -> opt

  module type FeatureRequests = sig

    val has_flag :
      ?argsource:(string array*int) ->
      ?prefix:string ->
      string (** flag *) ->
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
      'x (** default value *) ->
      'x
    val get_option_d' :
      ?argsource:(string array*int) ->
      ?optprefix:string ->
      ?optsep:string ->
      'x named_option ->
      (unit -> 'x) (** default value producer *) ->
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
          (constant (`Process_next false))
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
           state := `CaptureNext;
           `Process_next true
        | `Init ->
           (match String.chop_prefix marker_eq arg with
            | Some arg -> result := Some (f arg); `Process_next false
            | None -> `Process_next true)
        | `CaptureNext -> (state := `Init; result := Some (f arg)); `Process_next false in
      parse_opts_args ?argsource ~optprefix:"" ?optsep [par; constant (`Process_next false)] () |> ignore;
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
          if record_arg then (refappend args arg; loop (succ n) record_arg)
          else if arg = optsep then loop (succ n) true
          else loop (succ n) record_arg
        end in
    loop startidx false

end

module FmtPervasives = struct
  type ppf = Format.formatter

  let color_enabled = ref true

  let fprintf ppf fmt = Format.fprintf ppf fmt
  let printf fmt = Format.printf fmt
  let sprintf fmt = Format.asprintf fmt
  let eprintf fmt = Format.eprintf fmt

  module Fmt = struct

    let stdout_ppf = Format.std_formatter
    let stderr_ppf = Format.err_formatter
    let null_ppf = Format.formatter_of_out_functions {
          out_string = (fun _ _ _ -> ());
          out_flush = (fun _ -> ());
          out_newline = (fun _ -> ());
          out_spaces = (fun _ -> ());
          out_indent = (fun _ -> ());
        }

    let colored ?style ?color_mode:(m=`Fg) color ppf fmt =
      if !color_enabled then (
        let code_table = function
          (* `Color -> (fg_code, bg_code) *)
          | `Black -> 30, 40
          | `Red -> 31, 41
          | `Green -> 32, 42
          | `Yellow -> 33, 43
          | `Blue -> 34, 44
          | `Magenta -> 35, 45
          | `Cyan -> 36, 46
          | `White -> 37, 47

          | `Bright_black -> 90, 100
          | `Bright_red -> 91, 101
          | `Bright_green -> 92, 102
          | `Bright_yellow -> 93, 103
          | `Bright_blue -> 94, 104
          | `Bright_magenta -> 95, 105
          | `Bright_cyan -> 96, 106
        in
        let style_table = function
          | `Bold -> 1
          | `Thin -> 2
          | `Italic -> 3
          | `Underline -> 4
        in
        let esc x = "\027"^x in
        let reset = "[0m" in
        let color_code =
          code_table color
          |> (match m with `Fg -> fst | `Bg -> snd)
          |> sprintf "[%dm" in
        let style_code = style |> function
                                 | None -> None
                                 | Some s -> style_table s |> sprintf "[%dm" |> Option.some in

        (* start *)
        Format.fprintf ppf "@<0>%s"
          ((esc color_code)^(style_code |> Option.map esc |? ""));

        (* contents *)
        Format.kfprintf (fun ppf ->
            (* end *)
            Format.fprintf ppf "@<0>%s" (esc reset))
          ppf

          (* contents *)
          fmt
      ) else Format.fprintf ppf fmt
  end

  let condformat cond fmtfunc fmt =
    if cond then fmtfunc fmt
    else Format.ifprintf Fmt.null_ppf fmt

  let pp_of_to_string to_string ppf x =
    Format.pp_print_string ppf (to_string x)
  let to_string_of_pp pp = sprintf "%a" pp

  let pps to_string = pp_of_to_string to_string
  let spp pp = to_string_of_pp pp

  let pp_int = Format.pp_print_int
  let pp_float = Format.pp_print_float
  let pp_string = Format.pp_print_string
  let pp_char = Format.pp_print_char
  let pp_bool = Format.pp_print_bool
  let pp_unit ppf () = pp_string ppf "unit"
  let pp_ref_address ppf (r : 'x ref) =
    fprintf ppf "%#x" (2*(Obj.magic r))

  let pp_int32 ppf x =
    Int32.to_string x |> pp_string ppf
  let pp_int64 ppf x =
    Int64.to_string x |> pp_string ppf

  (** print integer with thousand separator *)
  let pp_integer_sep' ~padding ppf x =
    let rec loop acc x =
      if x > 0 then loop ((x mod 1000) :: acc) (x / 1000)
      else acc in
    let chunks = loop [] (abs x) in
    let chunks = match chunks with
      | [x] -> [string_of_int x]
      | h :: r -> string_of_int h :: (r |&> sprintf "%03d")
      | [] -> ["0"] in
    if x < 0 then pp_char ppf '-';
    let str = String.concat "," chunks in
    (match padding with
     | None -> ()
     | Some (0, _) -> ()
     | Some (d, pad) ->
        let d = d + (Float.ceil (float_of_int d /. 3.) |> int_of_float) - 1 in
        let slen = String.length str in
        if d > slen
        then Fn.ntimes (d-slen) (fun() -> pp_char ppf pad) ());
    pp_string ppf str

  let pp_integer_sep ppf = pp_integer_sep' ~padding:None ppf

  let pp_multiline ppf str =
    let open Format in
    let rec loop = function
      | [line] -> pp_string ppf line
      | line :: rest ->
         pp_string ppf line;
         pp_force_newline ppf ();
         loop rest
      | [] -> () in
    String.split_on_char '\n' str
    |> loop

  let pp_exn ppf exn =
    Printexc.to_string exn
    |> Format.pp_print_string ppf

  let pp_full_exn' ppf (exn, bt) =
    Format.fprintf ppf "@<2>%s@[<hov>@\n%a@]"
      (Printexc.to_string exn)
      pp_multiline
      Printexc.(bt |> raw_backtrace_to_string)

  let pp_full_exn ppf exn =
    pp_full_exn' ppf (exn, Printexc.(get_raw_backtrace()))


  let string_of_symbolic_output_items
      : Format.symbolic_output_item list -> string =
    fun items ->
    let buf = Buffer.create 0 in
    items |!> (function
      | Output_flush -> ()
      | Output_newline -> Buffer.add_char buf '\n'
      | Output_string str -> Buffer.add_string buf str
      | Output_spaces n | Output_indent n
        -> Buffer.add_string buf (String.make n ' '));
    Buffer.contents buf

end include FmtPervasives

module Log0 = struct

  open Format

  module Internals = struct
    let timestamp_func = ref (constant None)
    let logging_formatter = ref err_formatter
  end open Internals

  module LoggingConfig = struct
    let install_timestamp_function func = timestamp_func := func
    let set_logging_formatter ppf = logging_formatter := ppf
    let get_logging_formatter() = !logging_formatter
  end

  let logr fmt = fprintf !logging_formatter fmt

  let log ~label ?modul
        ?header_style:(style=None)
        ?header_color:(color=`Magenta)
        fmt =
    let header = match modul with
      | None -> label
      | Some m -> label^":"^m in
    let header = match !timestamp_func() with
      | None -> sprintf "[%s]" header
      | Some ts -> sprintf "[%s :%.3f]" header ts in
    let pp_header ppf =
      Fmt.colored ?style color ppf "%s" in
    logr "@<1>%s @[<hov>" (asprintf "%a" pp_header header);
    kfprintf (fun ppf -> fprintf  ppf "@]@.")
      !logging_formatter fmt

  let verbose ?modul fmt = log ?modul fmt ~label:"VERBOSE" ~header_style:(Some `Thin) ~header_color:`Bright_cyan
  let info ?modul fmt = log ?modul fmt ~label:"INFO" ~header_style:(Some `Bold) ~header_color:`Bright_cyan
  let warn ?modul fmt = log ?modul fmt ~label:"WARN" ~header_style:(Some `Bold) ~header_color:`Yellow
  let debug ?modul fmt = log ?modul fmt ~label:"DEBUG" ~header_style:(Some `Bold) ~header_color:`Magenta
  let error ?modul fmt = log ?modul fmt ~label:"ERROR" ~header_style:(Some `Bold) ~header_color:`Red

  module Pervasives = struct
    let debug ?modul fmt = debug ?modul fmt
    let info ?modul fmt = info ?modul fmt
  end

end

include Log0.Pervasives

module Json : sig
  type jv = [
    | `null
    | `bool of bool
    | `num of float
    | `str of string
    | `arr of jv list
    | `obj of (string*jv) list
    ]
  type legacy = [
    | `arr of jv list
    | `obj of (string*jv) list
    ]
  val of_legacy : legacy -> jv
  val to_legacy : jv -> legacy option

  (** Yojson.Safe.t *)
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
  val of_yojson : yojson -> jv
  val to_yojson : jv -> yojson

  (** Yojson.Basic.t *)
  type yojson' = ([
      | `Null
      | `Bool of bool
      | `Int of int
      | `Float of float
      | `String of string
      | `Assoc of (string * 't) list
      | `List of 't list
    ] as 't)
  val yojson_basic_of_safe : yojson -> yojson'
  val yojson_safe_of_basic : yojson' -> yojson

  type jsonm = jsonm_token seq
  and jsonm_token = [
    | `Null
    | `Bool of bool
    | `String of string
    | `Float of float
    | `Name of string
    | `As
    | `Ae
    | `Os
    | `Oe
    ]
  type 'loc jsonm' = ('loc*jsonm_token) seq
  type 'loc jsonm_pe (* pe = parsing_error *) = [
    | `empty_document
    | `premature_end of 'loc
    (** with loc of the starting token of the inner-most structure (viz. array/object) *)
    | `expecting_value_at of 'loc
    | `unexpected_token_at of 'loc*jsonm_token
    ]

  val of_jsonm' : 'loc jsonm' -> (jv * 'loc jsonm', 'loc jsonm_pe) result
  val of_jsonm : jsonm -> (jv * jsonm) option
  val to_jsonm : jv -> jsonm
end = struct
  type jv = [
    | `null
    | `bool of bool
    | `num of float
    | `str of string
    | `arr of jv list
    | `obj of (string*jv) list
    ]
  type legacy = [
    | `arr of jv list
    | `obj of (string*jv) list
    ]
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
  let of_legacy x = (x :> jv)
  let to_legacy : jv -> legacy option = function
    | #legacy as x -> Some x
    | _ -> None
  let rec of_yojson : yojson -> jv =
    function
    | `Null -> `null
    | `Bool x -> `bool x
    | `Int x -> `num (float_of_int x)
    | `Intlit x -> `num (float_of_string x)
    | `Float x -> `num x
    | `String x -> `str x
    | `Assoc x -> `obj (x |&> ?>of_yojson)
    | `List x -> `arr (x |&> of_yojson)
    | `Tuple x -> `arr (x |&> of_yojson)
    | `Variant (t, Some x) -> `arr [`str t; of_yojson x]
    | `Variant (t, None) -> `str t
  let rec to_yojson : jv -> yojson =
    function
    | `null -> `Null
    | `bool x -> `Bool x
    | `num x -> (
      if Float.is_integer x
         && (x <= (Int.max_int |> float_of_int))
         && (x >= (Int.min_int |> float_of_int))
      then (`Int (Float.to_int x))
      else `Float x)
    | `str x -> `String x
    | `arr x -> `List (x |&> to_yojson)
    | `obj x -> `Assoc (x |&> ?>to_yojson)

  type yojson' = ([
      | `Null
      | `Bool of bool
      | `Int of int
      | `Float of float
      | `String of string
      | `Assoc of (string * 't) list
      | `List of 't list
    ] as 't)
  let rec yojson_basic_of_safe : yojson -> yojson' = fun yojson ->
    match yojson with
    | `Null -> `Null
    | `Bool x -> `Bool x
    | `Int x -> `Int x
    | `Intlit x -> `Int (int_of_string x)
    | `Float x -> `Float x
    | `String x -> `String x
    | `Assoc xs -> `Assoc (xs |&> fun (n, x) -> (n, yojson_basic_of_safe x))
    | `List xs -> `List (xs |&> yojson_basic_of_safe)
    | `Tuple xs -> `List (xs |&> yojson_basic_of_safe)
    | `Variant (c, x_opt) ->
      begin match Option.map yojson_basic_of_safe x_opt with
        | None -> `List [`String c]
        | Some x -> `List [`String c; x]
      end
  let yojson_safe_of_basic : yojson' -> yojson = fun x ->
    (x :> yojson)

  type jsonm = jsonm_token seq
  and jsonm_token = [
    | `Null
    | `Bool of bool
    | `String of string
    | `Float of float
    | `Name of string
    | `As
    | `Ae
    | `Os
    | `Oe
    ]
  type atomic_jsonm_token = [
    | `Null
    | `Bool of bool
    | `String of string
    | `Float of float
    ]
  type value_starting_jsonm_token = [
    | atomic_jsonm_token
    | `As | `Os
    ]
  type 'loc jsonm' = ('loc*jsonm_token) seq

  type 'loc jsonm_pe (* pe = parsing_error *) = [
    | `empty_document
    | `premature_end of 'loc
    (** with loc of the starting token of the inner-most structure (viz. array/object) *)
    | `expecting_value_at of 'loc
    | `unexpected_token_at of 'loc*jsonm_token
    ]

  let of_jsonm' : 'loc jsonm' -> (jv*'loc jsonm', 'loc jsonm_pe) result =
    fun input ->
    let (>>=) m f = Result.bind m f in
    let jv_of_atom : atomic_jsonm_token -> jv = function
      | `Null -> `null
      | `Bool x -> `bool x
      | `String x -> `str x
      | `Float x -> `num x
      | _ -> . in
    let with_next
          (sloc : 'loc)
          (next : 'loc jsonm')
          (kont : 'loc -> 'loc jsonm' -> jsonm_token
                  -> 'r)
        : 'r
      = match next() with
      | Seq.Nil -> Error (`premature_end sloc)
      | Seq.Cons ((nloc, ntok), next') ->
         kont nloc next' ntok in
    let ok next x : (jv*'loc jsonm', 'loc jsonm_pe) result =
      Ok (x, next) in
    let rec value loc next =
      function
      | #atomic_jsonm_token as tok ->
         jv_of_atom tok |> ok next
      | `As -> with_next loc next (collect_array [])
      | `Os -> with_next loc next (collect_object [])
      | #value_starting_jsonm_token -> . (* assert that all value starting tokens are handled *)
      | (`Name _ | `Ae | `Oe) as tok ->
         Error (`unexpected_token_at (loc, tok))
      | _ -> .
    and collect_array acc sloc next = function
      | `Ae -> ok next (`arr (List.rev acc))
      | #value_starting_jsonm_token ->
         with_next sloc next value >>= (fun (v, next) ->
          with_next sloc next (fun _nloc ->
              collect_array (v :: acc) sloc))
      | (`Name _ | `Oe) as tok ->
         Error (`unexpected_token_at (sloc, tok))
      | _ -> .
    and collect_object acc sloc next = function
      | `Oe -> ok next (`obj (List.rev acc))
      | `Name key -> (
        with_next sloc next value >>= (fun (v, next) ->
          with_next sloc next (fun _nloc ->
              collect_object ((key, v) :: acc) sloc)))
      | (#value_starting_jsonm_token | `Ae) as tok ->
         Error (`unexpected_token_at (sloc, tok))
      | _ -> .
    in
    match input () with
    | Seq.Nil -> Error (`empty_document)
    | Seq.Cons ((loc, tok), next) -> (
      value loc next tok)
  let of_jsonm : jsonm -> (jv * jsonm) option = fun jsonm ->
    Seq.map (fun tok -> ((), tok)) jsonm
    |> of_jsonm'
    |> Result.to_option
    |> Option.map (fun (out, rest) -> (out, Seq.map snd rest))
  let rec to_jsonm : jv -> jsonm = function
    (* XXX - optimize *)
    | `null -> Seq.return `Null
    | `bool x -> Seq.return (`Bool x)
    | `num x -> Seq.return (`Float x)
    | `str x -> Seq.return (`String x)
    | `arr xs ->
      Seq.cons `As
        (List.fold_right (fun x seq ->
             Seq.append (to_jsonm x) seq)
            xs (Seq.return `Ae))
    | `obj xs ->
      Seq.cons `Os
        (List.fold_right (fun (name, x) seq ->
            Seq.append (Seq.cons (`Name name) (to_jsonm x)) seq)
            xs (Seq.return `Oe))
end
