[%%define mel (os_type = "mel")]

let refset r x = r := x
(** [refset r x] sets [x] to ref [r]. *)

let refget r = !r
(** [refget r] returns [!r]. *)

let refupdate r f = r := f !r
(** [refupdate r f] updates referent of [r] by [f]. *)

let refupdate_and_calc : 'x ref -> ('x -> 'a * 'x) -> 'a =
  fun r f ->
  let a, x' = f !r in
  r := x'; a
(** [refupdate_and_calc r f]
    calculate a result and the a updated referent value
    from the current referent value of [r] using [f]. *)

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

let iotaf func n =
  let rec loop acc = function
    | m when m = n -> acc
    | m -> loop (func m :: acc) (succ m) in
  loop [] 0 |> List.rev

let iotaf' func n =
  let rec loop = function
    | m when m = n -> ()
    | m -> func m; loop (succ m) in
  loop 0

let iotafl binop acc0 n =
  let rec loop acc = function
    | m when m = n -> acc
    | m -> loop (binop acc m) (succ m) in
  loop acc0 0

let iotafl' binop acc0 g n =
  let rec loop acc = function
    | m when m = n -> acc
    | m -> loop (binop acc (g m)) (succ m) in
  loop acc0 0

let min_by f x y = if f y > f x then x else y
let max_by f x y = if f y < f x then x else y

let dup : 'x -> 'x*'x = fun x -> x, x
let swap : 'x*'y -> 'y*'x = fun (x, y) -> y, x

module Functionals = struct
  let negate pred x = not (pred x)
  (** negate a predicate *)

  let both p g x = p x && g x
  let either p g x = p x || g x

  let dig2nd f a b = f b a
  (** [f] dig the second argument of [f] to be the first. aka [flip] *)

  let dig3rd f c a b = f a b c
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
      else loop (f y) in
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
    | Some 0 -> failwith "fixpoint not reached after 0 tries"
    | Some maxn ->
       fun f x ->
       let rec loop n (x,x') =
         if x = x' then x
         else if n = 0 then failwith' "fixpoint not reached after %d tries" maxn
         else loop (pred n) (x', f x') in
       loop (pred maxn) (x, f x)
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
    (** function application; reverse of [(|>)], same as [(@@)] *)
    let (&) : ('x -> 'y) -> 'x -> 'y = fun f x -> f x

    (** function composition 1 *)
    let (%) : ('y -> 'z) -> ('x -> 'y) -> ('x -> 'z) =
      fun f g x -> x |> g |> f

    (** function composition 1, on second argument *)
    let (%%) : ('a -> 'y -> 'z) -> ('x -> 'y) -> ('a -> 'x -> 'z) =
      fun f g x y -> f x (g y)

    (** function composition 2 *)
    let (&>) : ('x -> 'y) -> ('y -> 'z) -> ('x -> 'z) =
      fun g f x -> x |> g |> f

    let (?.) : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
      = dig2nd

    let (?..) : ('a -> 'b -> 'c -> 'd) -> 'c -> 'a -> 'b -> 'd
      = dig3rd

    let (!.) : 'b -> ('a -> 'b -> 'c) -> 'b -> 'c
      = fix2nd

    let (!..) : 'c -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'd
      = fix3rd

    (** function composition 2, arity=2 *)
    let (&&>) : ('x -> 'y -> 'z) -> ('z -> 'r) -> ('x -> 'y -> 'r) =
      fun g f x y -> g x y |> f

    (** piping with tapping *)
    let (|->) : 'x -> ('x -> unit) -> 'x = fun x f -> (f x); x

    let (//) : ('a -> 'x) -> ('b -> 'y) -> ('a*'b -> 'x*'y) =
      fun fa fb (a, b) -> fa a, fb b

    (** piping calc on snd *)
    let (/>) : 'a*'b -> ('b -> 'c) -> 'a*'c =
      fun (a, b) f -> a, f b

    (** piping calc on fst *)
    let (/<) : 'a*'b -> ('a -> 'c) -> 'c*'b =
      fun (a, b) f -> f a, b

    (** piping calc to snd *)
    let (|+>) : 'a -> ('a -> 'b) -> 'a * 'b =
      fun x f -> x, f x

    (** piping calc to fst *)
    let (|+<) : 'a -> ('a -> 'b) -> 'b * 'a =
      fun x f -> f x, x

    (** lift to snd *)
    let (?>) : ('b -> 'c) -> ('a*'b -> 'a*'c) =
      fun f -> fun (a, b) -> a, f b

    (** lift to fst *)
    let (?<) : ('a -> 'c) -> ('a*'b -> 'c*'b) =
      fun f -> fun (a, b) -> f a, b

    (** calculate to snd *)
    let (?+>) : ('a -> 'b) -> 'a -> 'a * 'b =
      fun f a -> a, f a

    (** calculate to fst *)
    let (?+<) : ('a -> 'b) -> 'a -> 'b * 'a =
      fun f a -> f a, a

    (** lift to map snd *)
    let (?&>) : ('y2 -> 'x2) -> ('x1 * 'x2 -> 'r) -> 'x1 * 'y2 -> 'r =
      fun g f -> fun (x, y) -> f (x, g y)

    (** lift to map fst *)
    let (?&<) : ('y1 -> 'x1) -> ('x1 * 'x2 -> 'r) -> 'y1 * 'x2 -> 'r =
      fun g f -> fun (x, y) -> f (g x, y)

    (** uncurry *)
    let (!!) : ('a -> 'b -> 'x) -> ('a*'b -> 'x) =
      fun f -> fun (a, b) -> f a b

    (** curry *)
    let (!?) : ('a*'b -> 'x) -> ('a -> 'b -> 'x) =
      fun f -> fun a b -> f (a, b)
  end

  module CommonTypes = struct
    type 'x endo = 'x -> 'x
    type null = |
  end

  module Infix = BasicInfix
end
module Fn = Functionals
include Functionals.BasicInfix
include Functionals.CommonTypes

module type Pipeable = sig
  type _ t
  val map : ('x -> 'y) -> 'x t -> 'y t
  val concat_map : ('x -> 'y t) -> 'x t -> 'y t
  val iter : ('x -> unit) -> 'x t -> unit
  val fold_left : ('acc -> 'x -> 'acc) -> 'acc -> 'x t -> 'acc
  val filter : ('x -> bool) -> 'x t -> 'x t
  val filter_map : ('x -> 'y option) -> 'x t -> 'y t
end

module type Pipeable_flat_map = sig
  type _ t
  val map : ('x -> 'y) -> 'x t -> 'y t
  val flat_map : ('x -> 'y t) -> 'x t -> 'y t
  val iter : ('x -> unit) -> 'x t -> unit
  val fold_left : ('acc -> 'x -> 'acc) -> 'acc -> 'x t -> 'acc
  val filter : ('x -> bool) -> 'x t -> 'x t
  val filter_map : ('x -> 'y option) -> 'x t -> 'y t
end

module type PipeOpsS = sig

  type 'x pipeable

  (** piping map *)
  val ( |&> ) : 'x pipeable -> ('x -> 'y) -> 'y pipeable

  (** piping flat-map *)
  val ( |&>> ) : 'x pipeable -> ('x -> 'y pipeable) -> 'y pipeable

  (** piping map to snd *)
  val ( |+&> ) : 'x pipeable -> ('x -> 'y) -> ('x * 'y) pipeable

  (** piping iter *)
  val ( |!> ) : 'x pipeable -> ('x -> unit) -> unit

  (** piping and iter-tapping *)
  val ( |-!> ) : 'x pipeable -> ('x -> unit) -> 'x pipeable

  (** piping fold_left *)
  val ( |@> ) : 'x pipeable -> 'acc * ('acc * 'x -> 'acc) -> 'acc

  (** piping filter *)
  val ( |?> ) : 'x pipeable -> ('x -> bool) -> 'x pipeable

  (** piping filter map *)
  val ( |&?> ) : 'x pipeable -> ('x -> 'y option) -> 'y pipeable

  (** piping filter map to snd *)
  val ( |+&?> ) : 'x pipeable -> ('x -> 'y option) -> ('x * 'y) pipeable

end

(** {2} (Section name todo) *)

module type Monadic = sig
  type _ t
  val return : 'x -> 'x t
  val bind : 'x t -> ('x -> 'y t) -> 'y t
end

module type MonadOpsS = sig
  include Monadic

  (** monadic binding *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (** monadic sequencing

      {b NB}
      if [expr1] and [expr2] both incur side effects,
      [expr1 >> expr2] will usually incur side effects of [expr2] first
      and then [expr1], which is usually not what the programmer expects *)
  val ( >> ) : 'x t -> 'y t -> 'y t

  (** monadic mapping *)
  val ( >|= ) : 'x t -> ('x -> 'y) -> 'y t

  (** monadic version of {!constant} *)
  val returning : 'a -> 'b -> 'a t

  val mlift : ('a -> 'b) -> ('a -> 'b t)
  val mwrap : ('a -> 'b) -> ('a t -> 'b t)
  val do_cond : bool -> ('a -> 'b t) -> ('a -> 'b t) -> 'a -> 'b t
  val do_if : bool -> ('a -> unit t) -> 'a -> unit t

  val sequence_list : 'a t list -> 'a list t

  (** monadic binding version of {!sequence_list} *)
  val ( >>=* ) : 'x t list -> ('x list -> 'y t) -> 'y t
end

module PipeOps(S : sig
             type _ t
             val map : ('x -> 'y) -> 'x t -> 'y t
             val concat_map : ('x -> 'y t) -> 'x t -> 'y t
             val iter : ('x -> unit) -> 'x t -> unit
             val fold_left : ('acc -> 'x -> 'acc) -> 'acc -> 'x t -> 'acc
             val filter : ('x -> bool) -> 'x t -> 'x t
             val filter_map : ('x -> 'y option) -> 'x t -> 'y t
           end) = struct
  open S

  (** piping map *)
  let (|&>) : 'x t -> ('x -> 'y) -> 'y t = fun xs f -> map f xs

  (** piping flat-map *)
  let (|&>>) : 'x t -> ('x -> 'y t) -> 'y t = fun xs f -> concat_map f xs

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

module PipeOps_flat_map(S : sig
             type _ t
             val map : ('x -> 'y) -> 'x t -> 'y t
             val flat_map : ('x -> 'y t) -> 'x t -> 'y t
             val iter : ('x -> unit) -> 'x t -> unit
             val fold_left : ('acc -> 'x -> 'acc) -> 'acc -> 'x t -> 'acc
             val filter : ('x -> bool) -> 'x t -> 'x t
             val filter_map : ('x -> 'y option) -> 'x t -> 'y t
           end) = struct
  include PipeOps(struct
    include S
    let concat_map : ('x -> 'y t) -> 'x t -> 'y t = S.flat_map
  end)
end

module Check_PipeOps : functor (S : Pipeable) -> PipeOpsS with type 'x pipeable := 'x S.t = PipeOps
module Check_PipeOps_flat_map : functor (S : Pipeable_flat_map) -> PipeOpsS with type 'x pipeable := 'x S.t = PipeOps_flat_map

module MonadOps(M : sig
             type _ t
             val return : 'x -> 'x t
             val bind : 'x t -> ('x -> 'y t) -> 'y t
           end)
       : MonadOpsS with type 'x t := 'x M.t = struct
  let return = M.return
  let bind = M.bind
  let (>>=) = M.bind
  let (>>) : 'x M.t -> 'y M.t -> 'y M.t =
    fun ma mb -> ma >>= fun _ -> mb
  let (>|=) : 'x M.t -> ('x -> 'y) -> 'y M.t =
    fun ma f -> ma >>= fun x -> return (f x)

  let returning x = fun _ -> return x

  let mlift = fun f x -> f x |> return

  let mwrap = fun f m -> m >>= mlift f

  let do_cond = fun c f1 f2 -> (if c then f1 else f2)

  let do_if = fun c f -> do_cond c f (returning ())

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

let projected_compare ?(cmp=compare) proj a b =
  cmp (proj a) (proj b)

let neg = Int.neg
let mul = Int.mul
let div = Int.div
let rem = Int.rem

module Either = struct

  [%%if ocaml_version < (4, 12, 0)]
  type ('a, 'b) t = Left of 'a | Right of 'b
  [%%else]
  include Either
  [%%endif]

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

module type ResultOfS = sig
  type err
  exception Error_result of err

  type 'x t = ('x, err) result
  val return : 'x -> 'x t
  val bind : 'x t -> ('x -> 'y t) -> 'y t
  val error : err -> _ t
end

module ResultOf(E : sig type err end)
       : ResultOfS with type err = E.err = struct
  type err = E.err
  exception Error_result of err

  type 'x t = ('x, err) result
  let bind : 'x t -> ('x -> 'y t) -> 'y t = Result.bind
  let return : 'x -> 'x t = Result.ok
  let error : err -> _ t = Result.error
end

module type ResultOfS' = sig
  include ResultOfS
  val get : 'x t -> 'x
end

module ResultOf'(E : sig
             type err
             val string_of_err : err -> string option
           end)
       : ResultOfS' with type err = E.err = struct
  include ResultOf(E)
  let () =
    let (>?) o f = Option.map f o in
    Printexc.register_printer (function
        | Error_result e -> E.string_of_err e >? Format.sprintf "Error_result(%s)"
        | _ -> None)
  let get : 'x t -> 'x = function
    | Ok x -> x
    | Error e -> raise (Error_result e)
end

module ResultWithErrmsg0 = struct
  include ResultOf'(struct type err = string let string_of_err = Option.some end)
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

module Option0 = struct
  include Option

  let return = some

  let get = function
    | Some x -> x
    | None -> raise Not_found

  let v default = function
    | Some x -> x
    | None -> default

  let v' gen_default = function
    | Some x -> x
    | None -> gen_default()

  let or_raise e = function
    | Some x -> x
    | None -> raise e

  let otherwise otherwise = function
    | Some x -> Some x
    | None -> otherwise

  let otherwise' otherwise_f = function
    | Some x -> Some x
    | None -> otherwise_f()

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

  (** [protect ~capture f x] returns [Some (f x)] except when
      - [f x] throws an [exn] s.t. [capture exn = true], it returns [None]
      - [f x] throws an [exn] s.t. [capture exn = false], it rethrows [exn]

      [~capture] defaults to [fun _exn -> true]
   *)
  let protect : ?capture:(exn -> bool) -> ('x -> 'y) -> 'x -> 'y option =
    fun ?(capture = constant true) f x ->
    try f x |> some
    with exn ->
      if capture exn then none
      else raise exn

  (** [try_make ~capture f] is [protect ~capture f ()], thus see {!protect}

      [~capture] defaults to [fun _exn -> true]
   *)
  let try_make : ?capture:(exn -> bool) -> (unit -> 'x) -> 'x option =
    fun ?capture f -> protect ?capture f ()

  (** a specialized version of [try_make] where [~capture] is fixed to
      [function Not_found -> true | _ -> false] *)
  let if_found : (unit -> 'x) -> 'x option =
    fun f -> try_make f ~capture:(function Not_found -> true | _ -> false)

end
module Option = struct
  include Option0
  module Ops_monad = MonadOps(Option0)
  module Ops = struct include Ops_monad end
end
let some = Option.some
let none = Option.none
let (>?) o f = Option.map f o
let (>>?) o f = Option.bind o f
let (|?) o v = Option.v v o
let (|?!) o v = Option.v' v o
let (||?) o1 o2 = Option.otherwise o2 o1
let (||?!) o1 o2 = Option.otherwise' o2 o1
let (&>?) : ('x -> 'y option) -> ('y -> 'z) -> ('x -> 'z option) =
  fun af f -> af &> (Option.map f)
let (&>>?) : ('x -> 'y option) -> ('y -> 'z option) -> ('x -> 'z option) =
  fun af g x -> af x >>? g

module Seq0 = struct
  include Seq

  [%%if ocaml_version >= (4, 13, 0)]
  module Ops_piping = PipeOps(Seq)
  [%%else]
  module Ops_piping = PipeOps_flat_map(Seq)
  [%%endif]
  open Ops_piping

  let bind m f = flat_map f m

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

  let iotaf f n = iota n |&> f

  let iotaf' f n = iota n |!> f

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
module Seq = struct
  include Seq0
  module Ops_monad = MonadOps(Seq0)
  module Ops = struct include Ops_piping include Ops_monad end
end
type 'x seq = 'x Seq.t

module Array0 = struct
  include Array

  let filter f arr =
    arr |> to_seq |> Seq.filter f |> of_seq

  let filter_map f arr =
    arr |> to_seq |> Seq.filter_map f |> of_seq

  let concat_map f arr =
    arr |> to_seq |> Seq.flat_map (f &> to_seq) |> of_seq

  include PipeOps(struct
              include Array
              let filter = filter
              let filter_map = filter_map
              let concat_map = concat_map
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

  let return x = make 1 x
  let bind ma f = concat_map f ma
end
module Array = struct
  include Array0
  module Ops_piping = PipeOps(Array0)
  module Ops_monad = MonadOps(Array0)
  module Ops = struct include Ops_piping include Ops_monad end
end

[%%if ocaml_version < (4, 14, 0)]
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
[%%endif]

module List0 = struct

  [%%if ocaml_version >= (4, 13, 0)]
  module Ops_piping = PipeOps(List)
  [%%else]
  let concat_map f l =
    let open List in
    let rec aux f acc = function
      | [] -> rev acc
      | x :: l ->
         let xs = f x in
         aux f (rev_append xs acc) l
    in aux f [] l
  module Ops_piping =
    PipeOps(struct
        include List
        let concat_map = concat_map
      end)
  [%%endif]
  include Ops_piping

  include List

  let conj_rev x xs = x :: xs |> rev

  let conj x xs' = conj_rev x (rev xs')

  let iota = function
    | 0 -> []
    | k -> 0 :: (List.init (pred k) succ)

  let iota1 = function
    | 0 -> []
    | k -> List.init k succ

  let range =
    let helper start end_ = iota (end_ - start) |&> (+) start in
    fun ?include_endpoint:(ie=false) ->
    if ie then (fun start end_ -> helper start (succ end_))
    else (fun start end_ -> helper start end_)

  let dedup' ~by l =
    let set = Hashtbl.create (List.length l) in
    l |?> (fun x ->
      if Hashtbl.mem set (by x) then false
      else (Hashtbl.add set (by x) true; true))

  let dedup l = dedup' ~by:identity l

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

  let deassoc_opt : 'k -> ('k*'v) list -> 'v option*('k*'v) list =
    fun k es ->
    let rec loop (ret, es) = function
      | [] -> ret, es
      | (k', v) :: rest when k' = k ->
         (* we are not shortcutting here since appending two lists is still O(n).. *)
         loop (some v, es) rest
      | e :: rest ->
         loop (ret, e :: es) rest in
    loop (none, []) es
  (** [deassoc_opt k l] removes entry keyed [k] from [l], interpreted as an association list,
      and return [v, l'] where [v] is the value of the entry being removed or [None], and
      [l'] is the list after the removal, or semantically unchanged if the key does not exist.
      note that entries in [l'] may differ in order wrt. [l].

      if there are multiple entries keyed [k], [v] will be [Some _] and [l'] will differ from the
      original, but otherwise the behavior is unspecified *)

  let deassq_opt : 'k -> ('k*'v) list -> 'v option*('k*'v) list =
    fun k es ->
    let rec loop (ret, es) = function
      | [] -> ret, es
      | (k', v) :: rest when k' == k ->
         (* we are not shortcutting here since appending two lists is still O(n).. *)
         loop (some v, es) rest
      | e :: rest ->
         loop (ret, e :: es) rest in
    loop (none, []) es
  (** same as [deassoc_opt] except using [(==)] when comparing keys *)

  let deassoc_opt' : 'k -> ('k*'v) list -> ('v*('k*'v) list) option =
    fun k es ->
    match deassoc_opt k es with
    | Some v, es -> Some (v, es)
    | None, _ -> None
  (** same as [deassoc_opt] but different return type *)

  let deassq_opt' : 'k -> ('k*'v) list -> ('v*('k*'v) list) option =
    fun k es ->
    match deassq_opt k es with
    | Some v, es -> Some (v, es)
    | None, _ -> None
  (** same as [deassq_opt] but different return type *)

  let deassoc : 'k -> ('k*'v) list -> 'v*('k*'v) list =
    fun k es ->
    let ov, es = deassoc_opt k es in
    Option.v' (fun() -> raise Not_found) ov, es
  (** same as [deassoc_opt] but throws [Not_found] when the requested key does not exist *)

  let deassq : 'k -> ('k*'v) list -> 'v*('k*'v) list =
    fun k es ->
    let ov, es = deassq_opt k es in
    Option.v' (fun() -> raise Not_found) ov, es
  (** same as [deassq_opt] but throws [Not_found] when the requested key does not exist *)

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

  let reduce_opt f = function
    | [] -> none
    | hd::tl -> foldl f hd tl |> some

  let min_opt cmp = function
    | [] -> none
    | hd::l ->
       let f acc x =
         if cmp acc x > 0 then x else acc in
       fold_left f hd l |> some

  let max_opt cmp = function
    | [] -> none
    | hd::l ->
       let f acc x =
         if cmp acc x < 0 then x else acc in
       fold_left f hd l |> some

  let min cmp = min_opt cmp &> Option.get

  let max cmp = max_opt cmp &> Option.get

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

  let and_last : 'x. 'x list -> 'x list*'x =
    fun xs ->
    match rev xs with
    | [] -> raise Not_found
    | l :: r -> rev r, l
  (** last element and rest of a list *)

  let iter' f f_last xs =
    let rec go = function
      | [x] -> f_last x
      | x :: rest ->
         f x; go rest
      | [] -> () in
    go xs

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
module List = struct
  include List0
  module Ops_monad = MonadOps(List0)
  module Ops = struct include Ops_piping include Ops_monad end
end
include List.Ops_piping

let iota = List.iota
let iota1 = List.iota1

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

module Bytes = struct
  include Bytes

  [%%if ocaml_version < (4, 13, 0)]
  let exists p s =
    let n = length s in
    let rec loop i =
      if i = n then false
      else if p (unsafe_get s i) then true
      else loop (succ i) in
    loop 0

  let for_all p s =
    let n = length s in
    let rec loop i =
      if i = n then true
      else if p (unsafe_get s i) then loop (succ i)
      else false in
    loop 0
  [%%endif]
end

module String = struct
  include String

  (** [partition_opt n s] returns [None] if [n] is greater than the length of [s] or
      [Some (s1, s2)] where [s = s1^s2 && n = length s1] otherwise *)
  let partition_opt n str =
    let len = length str in
    if n > len then None
    else Some (sub str 0 n, sub str n (len - n))

  (** [partition n s] returns [Some (s1, s2)] where [s = s1^s2 && n = length s1].

      {!Invalid_argument} will be thrown if [n] is greater than the length of [s] *)
  let partition n str =
    partition_opt n str
    |> Option.v' (fun () ->
           invalid_arg'
             "Kxclib.String.partition - n (= %d) is greater than the length (=%d) of %S"
             n (length str) str)

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

  let to_bytes = Bytes.of_string

  let to_list str =
    to_seq str |> List.of_seq

  let of_list = List.to_seq &> of_seq
  let of_array = Array.to_seq &> of_seq

  [%%if ocaml_version < (4, 13, 0)]
  let exists p s = Bytes.exists p (to_bytes s)
  let for_all p s = Bytes.for_all p (to_bytes s)
  [%%endif]

  [%%if ocaml_version < (4, 14, 0)]
  let is_valid_utf_8 : string -> bool =
    fun str ->
    let len = String.length str in
    let ch pos = String.get str pos |> int_of_char in
    let rec check_bytek = function
      | _, 0 -> true
      | pos, remaining ->
         pos < len
         && (ch pos land 0xc0 = 0x80)
         && check_bytek (succ pos, pred remaining) in
    let rec check pos =
      if pos = len then true else (
        let c = ch pos in
        if c land 0x80 = 0 then check (succ pos) (* 1byte utf8 *)
        else if c land 0xe0 = 0xc0 then (
          check_bytek (succ pos, 1) (* 2byte utf8 *)
          && check (pos+2)
        ) else if (c land 0xf0 = 0xe0) then (
          check_bytek (succ pos, 2) (* 3byte utf8 *)
          && check (pos+3)
        ) else if (c land 0xf8 = 0xf0) then (
          check_bytek (succ pos, 3) (* 4byte utf8 *)
          && check (pos+4)
        ) else false (* invalid *)
      )
    in check 0
  [%%endif]

  let valid_ascii_js_identifier s =
    let at = String.get s in
    let len = String.length s in
    let rec loop n =
      if n >= len then true
      else match at n with
        | '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
           loop (succ n)
        | _ -> false
    in
    if len = 0 then false
    else match at 0 with
      | '_' | 'a' .. 'z' | 'A' .. 'Z' ->
         loop 1
      | _ -> false

  let pp_json_escaped : Format.formatter -> string -> unit =
    fun ppf str ->
    let len = length str in
    let getc = unsafe_get str in
    let addc = Format.pp_print_char ppf in
    let adds = Format.pp_print_string ppf in
    let addu x =
      adds "\\u";
      adds (Format.sprintf "%04x" x) in
    let addb n k =
      iotaf' (fun i -> addc (getc (n + i))) k in
    let raise' pos =
      invalid_arg' "json_escaped: invalid/incomplete utf-8 string at pos %d" pos in
    let rec loop n =
      let adv k = loop (n + k) in
      let check k =
        if not (n + k <= len)
        then raise' (n+k-1) in
      if succ n > len then ()
      else (
        match getc n with
        | '"' -> adds "\\\""; adv 1
        | '\\' -> adds "\\\\"; adv 1
        | '\b' -> adds "\\b"; adv 1
        | '\012' -> adds "\\f"; adv 1
        | '\n' -> adds "\\n"; adv 1
        | '\r' -> adds "\\r"; adv 1
        | '\t' -> adds "\\t"; adv 1
        | '\127' -> addu 127; adv 1
        | c ->
           let x1 = int_of_char c in
           if x1 < 32 then (addu x1; adv 1)
           else if x1 < 127 then (addc c; adv 1)
           else (
             check 2;
             let spit k = check k; addb n k; adv k in
             if x1 land 0xe0 = 0xc0 then (
               spit 2 (* 2byte utf8 *)
             ) else if (x1 land 0xf0 = 0xe0) then (
               spit 3 (* 3byte utf8 *)
             ) else if (x1 land 0xf8 = 0xf0) then (
               spit 4 (* 4byte utf8 *)
             ) else raise' n
           )
      )
    in loop 0

  let json_escaped : string -> string =
    Format.asprintf "%a" pp_json_escaped

end

module MapPlus (M : Map.S) = struct
  let pp' kpp vpp ppf m =
    let open Format in
    fprintf ppf "{ ";
    pp_open_hovbox ppf 0;
    M.bindings m
    |> List.iter'
         (fun (key, value) ->
           fprintf ppf "@[<hov 0>%a=@,@[<hov 0>%a@];@]@;<1 2>@?"
             kpp key
             vpp value)
         (fun (key, value) ->
           fprintf ppf "@[<hov 0>%a=@,@[<hov 0>%a@];@] }@?"
             kpp key
             vpp value);
    pp_close_box ppf ()

  let of_list : (M.key * 'v) list -> 'v M.t =
    fun kvs -> kvs |> M.of_seq % List.to_seq
end

module StringMap = struct
  include Map.Make(String)
  include MapPlus(Map.Make(String))
  let pp vpp ppf m = pp' Format.pp_print_string vpp ppf m
end

module IntMap = struct
  include Map.Make(Int)
  include MapPlus(Map.Make(Int))
  let pp vpp ppf m = pp' Format.pp_print_int vpp ppf m
end

module Obj = struct
  include Obj

  [%%if ocaml_version < (5, 4, 0)]
  (* latest known version of OCaml using this implementation *)
  (* https://github.com/ocaml/ocaml/blob/5.3/runtime/hash.c#L313-L325 *)
  let hash_variant s =
    let accu = ref 0 in
    for i = 0 to String.length s - 1 do
      accu := 223 * !accu + Char.code s.[i]
    done;
    (* reduce to 31 bits *)
    accu := !accu land (1 lsl 31 - 1);
    (* make it signed for 64 bits architectures *)
    if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu
  [%%else]
  let hash_variant _s =
    failwith "Kxclib.Obj.hash_variant is not support for this OCaml version"
  [@@alert unavailable "hash_variant is not available for this OCaml version"]
  [%%endif]
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
  [@@warning "-48"]

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

module Int53p = struct
  type int53p_impl_flavor = [
    | `int_impl
    | `int64_impl
    | `float_impl
    | `custom_impl of string
    ]

  let pp_int53p_impl_flavor : Format.formatter -> int53p_impl_flavor -> unit = fun ppf ->
    let open Format in
    function
    | `int_impl -> pp_print_string ppf "int_impl"
    | `int64_impl -> pp_print_string ppf "int64_impl"
    | `float_impl -> pp_print_string ppf "float_impl"
    | `custom_impl s -> fprintf ppf "custom_impl(%s)" s
  let show_int53p_impl_flavor : int53p_impl_flavor -> string = Format.asprintf "%a" pp_int53p_impl_flavor

  module type Prims = sig
    type int53p
    val neg : int53p -> int53p
    val add : int53p -> int53p -> int53p
    val sub : int53p -> int53p -> int53p
    val mul : int53p -> int53p -> int53p
    val div : int53p -> int53p -> int53p
    val rem : int53p -> int53p -> int53p
  end

  module type Ops = sig
    type int53p

    val ( ~-% ) : int53p -> int53p
    val ( ~+% ) : int53p -> int53p

    val ( +% ) : int53p -> int53p -> int53p
    val ( -% ) : int53p -> int53p -> int53p
    val ( *% ) : int53p -> int53p -> int53p
    val ( /% ) : int53p -> int53p -> int53p
    val ( /%% ) : int53p -> int53p -> int53p (* rem *)
  end

  module type S = sig
    val impl_flavor : int53p_impl_flavor

    module Ops : Ops

    include Ops with type int53p = Ops.int53p
    include Prims with type int53p = int53p

    type t = Ops.int53p

    val zero : int53p
    val one : int53p
    val minus_one : int53p

    val succ : int53p -> int53p
    val pred : int53p -> int53p

    val abs : int53p -> int53p
    val equal : int53p -> int53p -> bool
    val compare : int53p -> int53p -> int
    val min : int53p -> int53p -> int53p
    val max : int53p -> int53p -> int53p

    val to_float : int53p -> float
    val of_float : float -> int53p
    val to_int : int53p -> int
    val of_int : int -> int53p
    val to_int64 : int53p -> int64
    val of_int64 : int64 -> int53p
    val to_nativeint : int53p -> nativeint
    val of_nativeint : nativeint -> int53p
    val to_string : int53p -> string
    val of_string : string -> int53p
  end

  module Internals = struct
    module MakeOps(M : Prims) : Ops with type int53p = M.int53p = struct
      type int53p = M.int53p
      let ( ~-% ) : int53p -> int53p = M.neg
      let ( ~+% ) : int53p -> int53p = identity
      let ( +% ) : int53p -> int53p -> int53p = M.add
      let ( -% ) : int53p -> int53p -> int53p = M.sub
      let ( *% ) : int53p -> int53p -> int53p = M.mul
      let ( /% ) : int53p -> int53p -> int53p = M.div
      let ( /%% ) : int53p -> int53p -> int53p = M.rem
    end

    module IntImpl : S = struct
      let impl_flavor = `int_impl

      include Int
      [%%if ocaml_version < (4, 13, 0)]
      let min (a: int) (b: int) = min a b
      let max (a: int) (b: int) = max a b
      [%%endif]

      module Ops = MakeOps(struct type int53p = int include Int end)
      include Ops
      let to_int = identity
      let of_int = identity
      let to_int64 = Int64.of_int
      let of_int64 = Int64.to_int
      let to_float = float_of_int
      let of_float = int_of_float

      [%%if not(mel)]
      let to_nativeint = Nativeint.of_int
      let of_nativeint = Nativeint.to_int
      [%%else]
      let to_nativeint = Int64.(to_nativeint % of_int)
      let of_nativeint = Int64.(to_int % of_nativeint)
      [%%endif]

      let of_string = int_of_string
    end

    module Int64Impl : S = struct
      let impl_flavor = `int64_impl

      include Int64
      [%%if ocaml_version < (4, 13, 0)]
      let min (a: int64) (b: int64) = min a b
      let max (a: int64) (b: int64) = max a b
      [%%endif]

      module Ops = MakeOps(struct type int53p = int64 include Int64 end)
      include Ops
      let of_int64 = identity
      let to_int64 = identity
    end

    module FloatImpl : S = struct
      let impl_flavor = `float_impl

      (* there is a problem with int_of_float in at least JSOO,
         and type int = float in both JSOO and BuckleScript runtime  *)
      let to_int, of_int = match Sys.backend_type with
        | Other "js_of_ocaml" | Other "BS" ->
           Obj.magic, Obj.magic
        | _ -> Float.(to_int, of_int)

      let round_towards_zero x =
        let open Float in
        if x < 0. then x |> neg % floor % neg
        else floor x

      module Float' = struct
        type t = float

        let zero = Float.zero
        let one = Float.one
        let minus_one = Float.minus_one
        let succ n = n +. 1.
        let pred n = n -. 1.
        let neg = Float.neg
        let add = Float.add
        let sub = Float.sub
        let mul = Float.mul
        let rem = Float.rem
        let abs = Float.abs
        let equal = Float.equal
        let compare = Float.compare
        let min = Float.min
        let max = Float.max

        let div a b = Float.div a b |> round_towards_zero

        let to_int = to_int
        let of_int = of_int

        let to_string f = f |> Int64.of_float |> Int64.to_string
      end
      include Float'
      module Ops = MakeOps(struct type int53p = float include Float' end)
      include Ops
      let of_float = identity
      let to_float = identity
      let to_int64 = Int64.of_float
      let of_int64 = Int64.to_float

      [%%if not(mel)]
      let to_nativeint = Nativeint.of_float
      let of_nativeint = Nativeint.to_float
      [%%else]
      let to_nativeint = Int64.(to_nativeint % of_float)
      let of_nativeint = Int64.(to_float % of_nativeint)
      [%%endif]

      let of_string = float_of_string
    end

    let current_impl_flavor =
      if Sys.int_size >= 53 then `int_impl
      else match Sys.backend_type with
           | Other "js_of_ocaml" | Other "BS" -> `float_impl
           | _ -> `int64_impl

    let impl_of_builtin_flavor : int53p_impl_flavor -> (module S) = function
      | `int_impl -> (module IntImpl)
      | `int64_impl -> (module Int64Impl)
      | `float_impl -> (module FloatImpl)
      | flavor -> failwith' "non-builtin int53p_impl_flavor: %a" pp_int53p_impl_flavor flavor

    module CurrentFlavorImpl = (val (impl_of_builtin_flavor current_impl_flavor))
  end

  include Internals.CurrentFlavorImpl

  type int53p = Internals.CurrentFlavorImpl.int53p

  let show_int53p = Int64.to_string % to_int64
  let pp_int53p ppf x =
    Format.(pp_print_string ppf (show_int53p x))

  type t = int53p
  [@@deriving show]
end
include Int53p.Ops

type int53p = Int53p.int53p
[@@deriving show]

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
  module EpochNormalizedTimestamp (_ : sig
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
  let pp_string_quoted ppf = Format.fprintf ppf "%S"
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

  type log_filter =
    | LogFilter_by_label_whitelist of string list
    | LogFilter_by_label_blacklist of string list
  [@@deriving show]

  let string_of_log_filter =
    to_string_of_pp pp_log_filter

  module Internals = struct
    let timestamp_func = ref (constant None)
    let logging_formatter = ref err_formatter
    let log_filter = ref (None : log_filter option)
    let _log_filter = ref (fun ~label:_ -> true : (label:string -> bool))
  end open Internals

  module LoggingConfig = struct
    let install_timestamp_function func = timestamp_func := func
    let set_logging_formatter ppf = logging_formatter := ppf
    let get_logging_formatter() = !logging_formatter
    let get_entry_filter() = !log_filter

    let set_entry_filter' =
      let module StringSet = Set.Make(String) in
      fun x ->
        refset log_filter x;
        refset _log_filter (x |> function
          | None -> fun ~label:_ -> true
          | Some (LogFilter_by_label_whitelist l) ->
            let s = StringSet.of_list l in
            fun ~label -> StringSet.mem label s
          | Some (LogFilter_by_label_blacklist l) ->
            let s = StringSet.of_list l in
            fun ~label -> not & StringSet.mem label s
        )

    let set_entry_filter = set_entry_filter' % some
  end

  let logr fmt = fprintf !logging_formatter fmt

  let log ~label ?modul
        ?header_style:(style=None)
        ?header_color:(color=`Magenta)
        fmt =
    if !_log_filter ~label then
      let header = match modul with
        | None -> label
        | Some m -> label^":"^m in
      let header = match !timestamp_func() with
        | None -> sprintf "[%s]" header
        | Some ts -> sprintf "[%s :%.3f]" header ts in
      let pp_header ppf =
        Fmt.colored ?style color ppf "%s" in
      logr "@<1>%s @[<hov>" (asprintf "%a" pp_header header);
      kfprintf (fun ppf -> fprintf ppf "@]@.")
        !logging_formatter fmt
    else
      ikfprintf ignore !logging_formatter fmt

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

module Runtime_info = struct
  type runtime_type =
    [ `classic_ocaml of [ `native | `bytecode ]
    | `js_of_ocaml
    | `buckle_script (** ReScript or Melange *)
    | `unknown of string
    ]

  let pp_runtime_type ppf : runtime_type -> unit =
    let print_string = pp_string ppf in
    function
    | `classic_ocaml `native -> print_string "classic_ocaml(native)"
    | `classic_ocaml `bytecode -> print_string "classic_ocaml(bytecode)"
    | `js_of_ocaml -> print_string "js_of_ocaml"
    | `buckle_script -> print_string "buckle_script"
    | `unknown x -> fprintf ppf "unknown(%s)" x

  let current_runtime_type : runtime_type =
    match Sys.backend_type with
    | Native -> `classic_ocaml `native
    | Bytecode -> `classic_ocaml `bytecode
    | Other "BS" -> `buckle_script
    | Other "js_of_ocaml" -> `js_of_ocaml
    | Other x -> `unknown x
end

type backtrace_info =
  [ `ocaml_backtrace of Printexc.raw_backtrace
  | `string_stacktrace of string
  ]
module Backtrace_info = struct
  type t = backtrace_info
  let pp : ppf -> t -> unit =
    fun ppf ->
    function
    | `ocaml_backtrace rb ->
       Format.fprintf ppf "%s" (Printexc.raw_backtrace_to_string rb)
    | `string_stacktrace s -> Format.fprintf ppf "%s" s
end

module type Io_style = sig
  (** exceptions thrown in executing second argument of [bind] is expected to be
      caught and could be retrieved using extract_error *)

  type 'x t
  val return : 'x -> 'x t
  val bind : 'x t -> ('x -> 'y t) -> 'y t

  val inject_error : exn -> 'x t
  val inject_error' : exn * backtrace_info option -> 'x t
  val extract_error : 'x t -> ('x, exn * backtrace_info option) result t

  val trace : string -> unit t
end

module Direct_io = struct
  type 'x t = ('x, exn * Backtrace_info.t) result

  let return : 'x -> 'x t = fun x -> Result.ok x

  [%%if mel]
  let inject_error' : exn * backtrace_info option -> 'x t =
    fun (exn, bt) ->
    let bt =
      match bt with
      | Some x -> some x
      | None -> (
        let stack = Js.Exn.asJsExn exn >>? Js.Exn.stack in
        match stack with
        | Some stack -> `string_stacktrace stack |> some
        | None -> none) in
    let bt = bt |? `string_stacktrace "*no_stacktrace*" in
    Result.error (exn, bt)
  [%%else]
  let inject_error' : exn * backtrace_info option -> 'x t =
    fun (exn, bt) ->
    let bt =
      bt |> Option.v' (fun () -> `ocaml_backtrace (Printexc.get_raw_backtrace ()))
    in
    Result.error (exn, bt)
  [%%endif]

  let inject_error : exn -> 'x t = fun exn -> inject_error' (exn, none)
  let extract_error : 'x t -> ('x, exn * backtrace_info option) result t
    = function
    | Ok x -> Ok x |> return
    | Error (e, bt) -> Error (e, some bt) |> return
  let trace s =
      Log0.log ~label:"trace" ~header_style:(Some `Thin) ~header_color:`Yellow
        "%s" s;
      Ok ()

  let bind : 'x t -> ('x -> 'y t) -> 'y t = fun x f ->
    try Result.bind x f
    with e -> inject_error e
end
module CheckDirectIo : Io_style = Direct_io

module Json : sig
  type jv = [
    | `null
    | `bool of bool
    | `num of float
    | `str of string
    | `arr of jv list
    | `obj of (string*jv) list
    ]

  type jv_field = string*jv
  type jv_fields = jv_field list

  type jv_kind = [
    | `null
    | `bool
    | `num
    | `str
    | `arr
    | `obj
    ]

  val classify_jv : jv -> jv_kind
  val string_of_jv_kind : jv_kind -> string

  val normalize : jv -> jv
  val normalize_fields : jv_fields -> jv_fields

  val eqv : jv -> jv -> bool
  (** whether two json value are equivalent, i.e. equal while ignoring ordering of object fields *)

  val pp_unparse : ppf -> jv -> unit
  (** [pp_unparse ppf j] output [j] as a JSON string.
      NB: this function does not check if [j] contains any [`str s]
           where [s] is an invalid UTF-8 string. it just assumes so. *)

  val unparse : jv -> string
  (** [unparse j] convert [j] to a JSON string using [pp_unparse].
      see [pp_unparse] for caveats. *)

  val pp_lit : ppf -> jv -> unit
  (** [pp_lit ppf j] output [j] in a format that can be used as an OCaml literal. *)

  val show : jv -> string
  (** [show j] convert [j] to a string using [pp_lit],
      which is a string that can be used as an OCaml literal. *)

  type jvpath = ([
    | `f of string (** field within an object *)
    | `i of int (** index within an array *)
    ] as 'path_component) list
  (** an empty path designate the root element *)

  val pp_jvpath : ppf -> jvpath -> unit
  (** print a human-friendly version of the jvpath. e.g.
      - [pp_jvpath []] prints [.] (a single dot)
      - [pp_jvpath [`f "foo"]] prints [.foo]
      - [pp_jvpath [`f "foo"; `f "bar"]] prints [.foo.bar]
      - [pp_jvpath [`f "foo"; `i 4]] prints [.foo[4]]
      - [pp_jvpath [`i 3]] prints [.[3]]
      - [pp_jvpath [`i 3; `i 4]] prints [.[3][4]]
      - [pp_jvpath [`i 3; `f "bar"]] prints [.[3].bar]
      - [pp_jvpath [`f "f!oo"]] prints [.["f!oo"]]
   *)

  val unparse_jvpath : jvpath -> string
  (** [unparse_jvpath path] converts [path] to string in the same syntax as {!pp_jvpath} *)

  val parse_jvpath' :
    ?check_root:[
      | `check_root_prefix (** default, check the prefix dot denoting the document root *)
      | `no_check (** do not care whether there is a prefix dot or not *)
      ]
    -> string
    -> (jvpath,
        [`pos of int | `premature_end of int]
       ) result
  (** parse jvpath conforming the syntax of {!pp_jvpath} *)

  val parse_jvpath_opt :
    ?check_root:[
      | `check_root_prefix (** default, check the prefix dot denoting the document root *)
      | `no_check (** do not care whether there is a prefix dot or not *)
      ]
    -> string
    -> jvpath option
  (** see {!parse_jvpath'} *)

  val parse_jvpath_exn :
    ?check_root:[
      | `check_root_prefix (** default, check the prefix dot denoting the document root *)
      | `no_check (** do not care whether there is a prefix dot or not *)
      ]
    -> string
    -> jvpath
  (** see {!parse_jvpath'} *)

  type legacy = [
    | `arr of jv list
    | `obj of (string*jv) list
    ]
  val of_legacy : legacy -> jv
  val to_legacy : jv -> legacy option

  (** Yojson.Safe.t *)
  type yojson = Yojson_compat.yojson
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

  module JCSnafi : sig
    val is_encodable_str : string -> bool
    val is_encodable_num : float -> bool
    val is_encodable : jv -> bool
    val compare_field_name : string -> string -> int
    val unparse_jcsnafi : jv -> string
  end
end = struct
  type jv = [
    | `null
    | `bool of bool
    | `num of float
    | `str of string
    | `arr of jv list
    | `obj of (string*jv) list
    ]
  let sort_by_key fs = fs |> List.sort_uniq (fun (k1, _) (k2, _) -> String.compare k1 k2)
  let rec eqv a b = match a, b with
    | `null, `null -> true
    | `bool a, `bool b -> a = b
    | `num a, `num b -> a = b
    | `str a, `str b -> a = b
    | `arr xs, `arr ys ->
       let rec loop = function
         | [], [] -> true
         | x :: xs, y :: ys when eqv x y -> loop (xs, ys)
         | _ -> false in
       loop (xs, ys)
    | `obj fs1, `obj fs2 ->
       let sort = sort_by_key in
       let fs1, fs2 = sort fs1, sort fs2 in
       let rec loop = function
         | [], [] -> true
         | (k1, x) :: xs, (k2, y) :: ys
              when k1 = k2 && eqv x y -> loop (xs, ys)
         | _ -> false in
       loop (fs1, fs2)
    | _ -> false

  type jv_field = string*jv
  type jv_fields = jv_field list

  type jv_kind = [
    | `null
    | `bool
    | `num
    | `str
    | `arr
    | `obj
    ]

  let classify_jv : jv -> jv_kind = function
    | `null -> `null
    | `bool _ -> `bool
    | `num _ -> `num
    | `str _ -> `str
    | `arr _ -> `arr
    | `obj _ -> `obj

  let string_of_jv_kind : jv_kind -> string = function
    | `null -> "null"
    | `bool -> "bool"
    | `num -> "number"
    | `str -> "string"
    | `arr -> "array"
    | `obj -> "object"

  let rec normalize : jv -> jv = function
    | (`null | `bool _ | `num _ | `str _) as x -> x
    | `arr xs -> `arr (xs |&> normalize)
    | `obj fs -> `obj (normalize_fields fs)
  and normalize_fields : jv_fields -> jv_fields = fun fs ->
    sort_by_key fs |&> (fun (k, v) -> k, normalize v)

  let rec pp_unparse = fun ppf ->
    let self = pp_unparse in
    let outs = Format.pp_print_string ppf in
    let outf fmt = Format.fprintf ppf fmt in
    function
    | `null -> outs "null"
    | `bool true -> outs "true"
    | `bool false -> outs "false"
    | `num n -> outf "%g" n
    | `str s -> outf "\"%a\"" String.pp_json_escaped s
    | `arr [] -> outs "[]"
    | `arr xs ->
       outs "[";
       xs |> List.iter'
               (fun j -> outf "%a,%!" self j)
               (fun j -> outf "%a]%!" self j)
    | `obj [] -> outs "{}"
    | `obj fs ->
       outs "{";
       fs |> List.iter'
               (fun (f,j) -> outf "\"%a\":%!%a,%!"
                               String.pp_json_escaped f
                               self j)
               (fun (f,j) -> outf "\"%a\":%!%a}%!"
                               String.pp_json_escaped f
                               self j)

  let unparse = sprintf "%a" pp_unparse

  let rec pp_lit = fun ppf ->
    let self = pp_lit in
    let outs = Format.pp_print_string ppf in
    let outf fmt = Format.fprintf ppf fmt in
    function
    | `null -> outs "`null"
    | `bool true -> outs "`bool true"
    | `bool false -> outs "`bool false"
    | `num n ->
       if n = 0. then outs "`num 0."
       else if n < 0. then outf "`num (%F)" n
       else outf "`num %F" n
    | `str s -> outf "`str %S" s
    | `arr [] -> outs "`arr []"
    | `arr xs ->
       outf "`arr [";
       xs
       |> List.iter'
         (fun value ->
           fprintf ppf "@[<hov 0>%a@];@;<1 2>@?"
             self value)
         (fun value ->
           fprintf ppf "@[<hov 0>%a@]]@?"
             self value);
    | `obj [] -> outs "`obj []"
    | `obj fs ->
       outf "`obj [";
       fs
       |> List.iter'
         (fun (key, value) ->
           fprintf ppf "@[<hov 0>%S, @,@[<hov 0>%a@];@]@;<1 2>@?"
             key
             self value)
         (fun (key, value) ->
           fprintf ppf "@[<hov 0>%S, @,@[<hov 0>%a@]@]]@?"
             key
             self value)

  let show = sprintf "%a" pp_lit

  type jvpath = ([
    | `f of string
    | `i of int
    ] as 'path_component) list

  module StringSet = Set.Make(String)

  let reserved_javascript_keywords = StringSet.of_list [
      "break"; "case"; "catch"; "class"; "const"; "continue";
      "debugger"; "default"; "delete"; "do"; "else"; "export";
      "extends"; "finally"; "for"; "function"; "if"; "import"; "in";
      "instanceof"; "new"; "return"; "super"; "switch"; "this";
      "throw"; "try"; "typeof"; "var"; "void"; "while"; "with"; "yield"; ]

  let pp_jvpath : ppf -> jvpath -> unit =
    fun ppf ->
    let outf fmt = fprintf ppf fmt in
    let outs string = Format.pp_print_string ppf string in
    let rec go : [`root of bool ] * jvpath -> unit = function
    | `root true, [] -> outs "."
    | `root false, [] -> ()
    | `root is_root, `f fname :: rest ->
       continue_fname is_root fname rest
    | `root is_root, `i idx :: rest ->
       if is_root then outs ".";
       outf "[%d]" idx; go (`root false, rest)
    | _ -> .
    and continue_fname forced_dot fname rest =
      let initial_char = function
        | '_' | 'a'..'z' | 'A'..'Z' -> true
        | _ -> false in
      let identifier_char = function
        | '0'..'9' | '_' | 'a'..'z' | 'A'..'Z' -> true
        | _ -> false in
      let needs_escape =
        let len = String.length fname in
        if len = 0 then true
        else if StringSet.mem fname reserved_javascript_keywords then true
        else if (
          (String.get fname 0 |> initial_char)
          && (String.for_all identifier_char fname)
        ) then false
        else true in
      if needs_escape then (
        if forced_dot then outs ".";
        outf "[\"%a\"]" String.pp_json_escaped fname;
        go (`root false, rest)
      ) else (
        outf ".%s" fname;
        go (`root false, rest)
      )
    in fun path -> go (`root true, path)

  let unparse_jvpath : jvpath -> string = sprintf "%a" pp_jvpath

  module ParseJvpathResult =
    ResultOf'(struct
        type err = [`pos of int | `premature_end of int]
        let string_of_err = function
          | `pos p -> sprintf "Bad_input(pos=%d)" p |> some
          | `premature_end p -> sprintf "Premature_end(pos=%d)" p |> some
      end)
  module ParseJvpathResult_ops = MonadOps(ParseJvpathResult)

  let parse_jvpath' : ?check_root:_ -> string -> (jvpath, [`pos of int | `premature_end of int]) result =
    let open Result in
    let open ParseJvpathResult_ops in
    fun ?(check_root = `check_root_prefix) input ->
    let len = String.length input in
    let sub start end_ = StringLabels.sub input ~pos:start ~len:(end_ - start) in
    let ch pos =
      if pos >= len then error (`premature_end pos)
      else String.get input pos |> ok in
    let next ~n pos =
      if n + pos > len then error (`premature_end pos)
      else ok (n + pos, sub pos (pos+n)) in
    let rec advance_whitespaces expect_non_whitespace pos =
      if not expect_non_whitespace && pos = len then ok pos else (
        ch pos >>= function
        | ' ' | '\t' | '\r' | '\n' | '\x0C' (* form feed *) ->
           advance_whitespaces expect_non_whitespace (succ pos)
        | _ -> ok pos) in
    let expect_ch c pos =
      if pos = len then error (`premature_end pos) else (
        ch pos >>= fun c' ->
        if c' = c then ok (succ pos)
        else error (`pos pos)
      ) in
    let expect_prefix prefix pos =
      let plen = String.length prefix in
      if plen + pos > len then (
        error (`premature_end pos)
      ) else if prefix <> StringLabels.sub input ~pos ~len:plen then (
        error (`pos pos)
      ) else ok (plen + pos) in
    let expect_prefix' prefix pos =
      advance_whitespaces (pos <> 0) pos >>= fun pos ->
      expect_prefix prefix pos in
    let rec continue_identifier pos0 pos =
      if pos = len then ok (pos, sub pos0 pos) else (
        ch pos >>= function
        | '0'..'9' | '_' | 'a'..'z' | 'A'..'Z' ->
           continue_identifier pos0 (succ pos)
        | _ -> ok (pos, sub pos0 pos)
      ) in
    let rec continue_numeric pos0 pos =
      if pos = len then ok (pos, sub pos0 pos) else (
        ch pos >>= function
        | '0'..'9' ->
           continue_numeric pos0 (succ pos)
        | _ -> ok (pos, sub pos0 pos)
      ) in
    let rec continue_quoted ?(buf = Buffer.create 8) pos =
      let addc c = Buffer.add_char buf c in
      let addu ucode = Uchar.of_int ucode |> Buffer.add_utf_8_uchar buf in
      if pos = len then error (`premature_end pos) else (
        let pos' = pos+2 in
        let addc' c = addc c; continue_quoted ~buf pos' in
        ch pos >>= function
        | '\\' -> (
          ch (succ pos) >>= function
          | '"' -> addc' '"'
          | '\\' -> addc' '\\'
          | 'b' -> addc' '\b'
          | 'f' -> addc' '\012'
          | 'n' -> addc' '\n'
          | 'r' -> addc' '\r'
          | 't' -> addc' '\t'
          | 'u' -> (
             next ~n:4 pos'
             >|= (?> (Option.protect (fun s -> Scanf.sscanf s "%04x%!" identity)))
             >>= function
             | pos, Some ucode ->
                addu ucode;
                continue_quoted ~buf pos
             | _ -> error (`pos pos'))
          | _ -> error (`pos pos'))
        | '"' ->
           ok (succ pos, Buffer.contents buf)
        | c ->
           addc c; continue_quoted ~buf (succ pos)
      ) in
    let rec go ctx acc pos =
      advance_whitespaces false pos >>= fun pos ->
      if pos = len then ok acc else (
        let err = error (`pos pos) in
        ch pos >|= (fun x -> ctx, x) >>= function
        | `root, '.' -> err
        | (`expect_fname | `root), ('_' | 'a'..'z' | 'A'..'Z') ->
           continue_identifier pos (succ pos) >>= fun (pos, fname) ->
           go `neutral (`f fname :: acc) pos
        | (`neutral | `root), '[' ->
           go `within_bracket acc (succ pos)
        | `neutral, '.' ->
           go `expect_fname acc (succ pos)
        | `within_bracket, '0'..'9' ->
           let pos0 = pos in
           continue_numeric pos (succ pos) >>= fun (pos, idx) ->
           int_of_string_opt idx |> Option.to_result ~none:(`pos pos0) >>= fun idx ->
           expect_ch ']' pos >>= fun pos ->
           go `neutral (`i idx :: acc) pos
        | `within_bracket, '"' ->
           continue_quoted (succ pos) >>= fun (pos, fname) ->
           expect_ch ']' pos >>= fun pos ->
           go `neutral (`f fname :: acc) pos
        | (`expect_fname | `root), _ -> err
        | _ -> err
      )
    in
    let go0 pos = go `root [] pos >|= List.rev in
    match check_root, len with
    | `check_root_prefix, _ ->
       expect_prefix' "." 0 >>= go0
    | `no_check, 0 -> ok []
    | `no_check, _ ->
       advance_whitespaces false 0 >>= fun pos ->
       if pos = len then ok []
       else (
         ch pos >>= function
         | '.' -> go0 (succ pos)
         | _ -> go0 (pos))

  let parse_jvpath_opt ?check_root = parse_jvpath' ?check_root &> Result.to_option

  let parse_jvpath_exn ?check_root = parse_jvpath' ?check_root &> ParseJvpathResult.get

  type legacy = [
    | `arr of jv list
    | `obj of (string*jv) list
    ]
  type yojson = Yojson_compat.yojson
  let of_legacy x = (x :> jv)
  let to_legacy : jv -> legacy option = function
    | #legacy as x -> Some x
    | _ -> None
  let rec of_yojson : [< yojson] -> jv =
    function[@warning "-11"]
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

  let rec yojson_basic_of_safe : [< yojson] -> yojson' = fun yojson ->
    match[@warning "-11"] yojson with
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
      | #value_starting_jsonm_token as head ->
         with_next sloc Seq.(cons (sloc, head) next) value >>= (fun (v, next) ->
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

  module JCSnafi = struct
    let min_fi_float = -. (2.0 ** 52.0)
    let max_fi_float = (2.0 ** 52.0) -. 1.0

    let iter_valid_uchar f str =
      let str_len = String.length str in
      let rec loop i =
        if i < str_len then
          let utf8dec = String.get_utf_8_uchar str i in

          if not (Uchar.utf_decode_is_valid utf8dec) then
            invalid_arg' "Invalid Unicode: %s" (String.escaped str)
          else
            Uchar.utf_decode_uchar utf8dec |> f;
          loop (i + Uchar.utf_decode_length utf8dec)
      in
      loop 0

    let utf16_bytes_of_string str =
      let buf = Buffer.create (String.length str) in
      str |> iter_valid_uchar (Buffer.add_utf_16be_uchar buf);
      Buffer.to_bytes buf

    let serialize_string_jcs ?buf str =
      let buf = buf |?! (fun () -> Buffer.create (String.length str)) in
      Buffer.add_string buf "\"";

      (if str |> String.for_all (fun c ->
                     (* check if directly printable *)
                     c != '\\' && c != '\"' &&
                       let x = Char.code c in
                       x >= 0x20 && x < 0x7f)

      then (* fast path *)
        Buffer.add_string buf str

      else (* otherwise.. *)
        str |> iter_valid_uchar (fun uchar ->
          (* ref: https://www.rfc-editor.org/rfc/rfc8785#section-3.2.2.2 *)
          match Uchar.to_int uchar with
          | 0x0008 -> Buffer.add_string buf {|\b|}
          | 0x0009 -> Buffer.add_string buf {|\t|}
          | 0x000A -> Buffer.add_string buf {|\n|}
          | 0x000C -> Buffer.add_string buf {|\f|}
          | 0x000D -> Buffer.add_string buf {|\r|}
          | ui when ui >= 0x0000 && ui <= 0x001F
            -> Buffer.add_string buf (Printf.sprintf "\\u%04x" ui)
          | 0x005C -> Buffer.add_string buf {|\\|}
          | 0x0022 -> Buffer.add_string buf {|\"|}
          | _ -> Buffer.add_utf_8_uchar buf uchar));

      Buffer.add_string buf "\"";
      buf

    let is_encodable_str str =
      match iter_valid_uchar (constant ()) str with
      | _ -> true
      | exception (Invalid_argument _) -> false

    let is_encodable_num f =
      f >= min_fi_float && f <= max_fi_float && Float.is_integer f

    let compare_field_name str1 str2 =
      Bytes.compare (utf16_bytes_of_string str1) (utf16_bytes_of_string str2)

    let is_ascii_str = String.for_all (fun c -> Char.code c <= 127)

    let is_all_ascii_property : (string * jv) list -> bool =
      List.for_all (fst &> is_ascii_str)

    let sort_obj_asserting_uniq es =
      let cmp = if is_all_ascii_property es then String.compare
                else compare_field_name in

      let cmp_asserting_uniq x y =
        match cmp x y with
        | 0 -> invalid_arg' "Duplicate property names: %s"
                 (serialize_string_jcs x |> Buffer.contents)
        | r -> r
      in
      List.sort (projected_compare ~cmp:cmp_asserting_uniq fst) es

    let rec is_encodable jv =
      match jv with
      | `null -> true
      | `bool _ -> true
      | `num n -> is_encodable_num n
      | `str s -> is_encodable_str s
      | `obj es -> (
        match
          (* check if all members encodables *)
          List.for_all (fun (k, v) -> is_encodable_str k && is_encodable v) es,
          (* check if keys are all unique *)
          sort_obj_asserting_uniq es
        with
        | (false, _
           | exception (Invalid_argument _)
          ) -> false
        | _ -> true
      )
      | `arr xs -> List.for_all is_encodable xs

    let rec unparse_jcsnafi0 ?buf jv : Buffer.t =
      let imm s =
        buf |?! (fun () -> Buffer.create (String.length s))
        |-> (Fn.flip Buffer.add_string s)
      in
      match jv with
      | `null -> imm "null"
      | `bool true -> imm "true"
      | `bool false -> imm "false"
      | `str s -> serialize_string_jcs ?buf s

      | `num n when is_encodable_num n ->
         imm (Int53p.to_string (Int53p.of_float n))
      | `num n ->
         invalid_arg'
           "Number cannot be safely encoded with Json_JCSnafi (encountering: %f)" n

      | `obj [] -> imm "{}" (* fast path *)
      | `obj ((k1, _) :: _ as es) -> (
        let buf = buf |?! (fun () -> Buffer.create (String.length k1)) in
        let es = sort_obj_asserting_uniq es in
        let per_entry (k, v) =
          serialize_string_jcs ~buf k |> ignore;
          Buffer.add_char buf ':';
          unparse_jcsnafi0 ~buf v |> ignore
        in
        Buffer.add_char buf '{';
        es |> List.iter'
                (per_entry &> (fun () -> Buffer.add_char buf ','))
                per_entry;
        Buffer.add_char buf '}';
        buf
      )

      | `arr [] -> imm "[]" (* fast path *)
      | `arr es ->
         let buf = buf |?! (fun () -> Buffer.create 8 (* that is, 64 bits *)) in
         Buffer.add_char buf '[';
         es |> List.iter'
                 (unparse_jcsnafi0 ~buf &> (fun _ -> Buffer.add_char buf ','))
                 (unparse_jcsnafi0 ~buf &> ignore);
         Buffer.add_char buf ']';
         buf

    let unparse_jcsnafi = unparse_jcsnafi0 &> Buffer.contents
  end

  (* signature check *)
  module _ : sig
    val is_encodable_str : string -> bool
    val is_encodable_num : float -> bool
    val is_encodable : jv -> bool
    val compare_field_name : string -> string -> int
    val unparse_jcsnafi : jv -> string
  end = JCSnafi
end

module Json_JCSnafi = Json.JCSnafi

module Jv : sig
  open Json

  val pump_field : string -> jv -> jv
  val pump_fields : string list -> jv -> jv

  val access : jvpath -> jv -> jv option
  val access_null : jvpath -> jv -> unit option
  val access_bool : jvpath -> jv -> bool option
  val access_num : jvpath -> jv -> float option
  val access_int : jvpath -> jv -> int option
  val access_int53p : jvpath -> jv -> int53p option
  val access_str : jvpath -> jv -> string option
  val access_arr : jvpath -> jv -> jv list option
  val access_obj : jvpath -> jv -> jv_fields option

  val access' : (jv -> 'a option) -> jvpath -> jv -> 'a option
  val access_arr' : (jv -> 'a option) -> jvpath -> jv -> 'a list option
end = struct
  open Json

  let pump_field fname : jv -> jv = function
    | `obj ([(_, _)] | []) as jv -> jv
    | `obj fs as jv -> (
      match List.deassoc_opt fname fs with
      | Some fval, fs' ->
         `obj ((fname, fval) :: fs')
      | None, _ -> jv)
    | jv -> jv

  module SSet = Set.Make(String)
  module SMap = Map.Make(String)

  let pump_fields fnames : jv -> jv = function
    | `obj ([(_, _)] | []) as jv -> jv
    | `obj fs as jv ->
       let fnset = SSet.of_list fnames in
       let len = SSet.cardinal fnset in
       let hit, miss =
         fs |@> (
           (SMap.empty, []),
           fun ((hit, miss), (fn, fv)) ->
           if SSet.mem fn fnset
           then SMap.add fn fv hit, miss
           else hit, (fn, fv) :: miss) in
       if SMap.cardinal hit = len
       then
         `obj
           (List.rev_append
              (fnames
               |> List.rev_map (fun fn ->
                      fn, SMap.find fn hit))
              miss)
       else jv
    | jv -> jv

  let access : jvpath -> jv -> jv option = fun path jv ->
    let rec go = function
      | [], x -> some x
      | `f fname :: path', `obj fs ->
         fs |> List.find_map (fun (k, v) ->
                   if k = fname then go (path', v)
                   else none
                 )
      | `i idx :: path', `arr xs ->
         List.nth_opt xs idx >>? (fun x -> go (path', x))
      | _ -> none
    in
    go (path, jv)

  let access' : (jv -> 'a option) -> jvpath -> jv -> 'a option = fun f path jv ->
    access path jv >>? f

  let access_null : jvpath -> jv -> unit option =
    access' (function
        | `null -> some ()
        | _ -> none)

  let access_bool : jvpath -> jv -> bool option =
    access' (function
        | `bool x -> some x
        | _ -> none)

  let access_num : jvpath -> jv -> float option =
    access' (function
        | `num x -> some x
        | _ -> none)

  let access_int : jvpath -> jv -> int option =
    access' (function
        | `num x when (float_of_int % int_of_float) x = x
          -> some (x |> int_of_float)
        | _ -> none)

  let access_int53p : jvpath -> jv -> int53p option = fun path jv ->
    access_num path jv >? Int53p.of_float

  let access_str : jvpath -> jv -> string option =
    access' (function
        | `str x -> some x
        | _ -> none)

  let access_arr : jvpath -> jv -> jv list option =
    access' (function
        | `arr xs -> some xs
        | _ -> none)

  let access_obj : jvpath -> jv -> jv_fields option =
    access' (function
        | `obj fs -> some fs
        | _ -> none)

  let access_arr' : (jv -> 'a option) -> jvpath -> jv -> 'a list option = fun f path jv ->
    let open Option.Ops_monad in
    access_arr path jv
    >? (List.map f &> sequence_list)
    |> Option.join
end

module Base64 = struct
  module type Config = sig
    (** the 62nd character. ['+'] in rfc4648, ['-'] in rfc4648_url. *)
    val c62 : char

    (** the 63rd character. ['/'] in rfc4648, ['_'] in rfc4648_url. *)
    val c63 : char

    (** the pad character. if [None], padding is disabled.

        [Some '='] in rfc4648. [None] in rfc4648_url. *)
    val pad : char option

    (** if set to true, validate padding length on decoding. *)
    val validate_padding: bool

    (** if set to true, newline characters are ignored on decoding. *)
    val ignore_newline : bool

    (** if set to true, unknown characters are ignored on decoding.

        [ignore_unknown = true] implies [ignore_newline = true]. *)
    val ignore_unknown : bool
  end

  module type T = sig
    (**
      Takes an input [bytes], and writes the encoded string to [Buffer.t].
      @param offset   the offset of input which the encoder should start reading from.
      @param len      the length of input which the encoder should read.
      @return the number of bytes written to [Buffer.t].
    *)
    val encode_buf : ?offset:int -> ?len:int -> Buffer.t -> bytes -> int (* written bytes*)

    (**
      Takes an input [string], and writes the decoded bytes to [Buffer.t].
      @param offset   the offset of input which the decoder should start reading from.
      @param len      the length of input which the decoder should read.
      @return the number of bytes written to [Buffer.t].
    *)
    val decode_buf : ?offset:int -> ?len:int -> Buffer.t -> string -> int (* written bytes *)

    (**
      Takes an input [bytes], and returns the encoded [string].
      @param offset   the offset of input which the encoder should start reading from.
      @param len      the length of input which the encoder should read.
    *)
    val encode : ?offset:int -> ?len:int -> bytes -> string

    (**
      Takes an input [string], and returns the decoded [bytes].
      @param offset   the offset of input which the decoder should start reading from.
      @param len      the length of input which the decoder should read.
    *)
    val decode : ?offset:int -> ?len:int -> string -> bytes
  end

  exception Invalid_base64_padding of [
    | `invalid_char_with_position of char * int
    | `invalid_padding_length of int
    ]

  let () =
    Printexc.register_printer begin function
    | Invalid_base64_padding (`invalid_char_with_position (c, i)) ->
      some (sprintf "Invalid_base64_padding - char %c at %d" c i)
    | Invalid_base64_padding (`invalid_padding_length len) ->
      some (sprintf "Invalid_base64_padding_len - %d" len)
    | _ -> none
    end

  module Make (C: Config) : T = struct
    open C

    let int_A = int_of_char 'A'
    let int_Z = int_of_char 'Z'
    let int_a = int_of_char 'a'
    let int_z = int_of_char 'z'
    let int_0 = int_of_char '0'
    let int_9 = int_of_char '9'
    let c62, c63 = int_of_char c62, int_of_char c63
    let sixbit_to_char b =
      if b < 26 (* A-Z *) then b + int_A
      else if b < 52 (* a-z *) then b - 26 + int_a
      else if b < 62 (* 0-9 *) then b - 52 + int_0
      else if b = 62 then c62
      else c63
    let char_to_sixbit c =
      if int_A <= c && c <= int_Z then Some (c - int_A)
      else if int_a <= c && c <= int_z then Some (c - int_a + 26)
      else if int_0 <= c && c <= int_9 then Some (c - int_0 + 52)
      else if c = c62 then Some 62
      else if c = c63 then Some 63
      else None

    let encode_buf ?(offset=0) ?len (output: Buffer.t) (input: bytes) =
      let input_offset, input_end, input_length =
        let orig_len = Bytes.length input in
        let len = len |? (orig_len - offset) in
        let end_index = offset + len in
        if len < 0 || end_index > orig_len then
          invalid_arg' "Base64.encode: the input range (offset:%d, len:%d) is out of bounds" offset len
        else offset, end_index, len
      in
      let output_buf =
        let estimated_chars = (input_length / 3) * 4 + 4 (* worst case: (4/3)n + 2 + "==" *) in
        Buffer.create estimated_chars
      in
      let write i o len =
        let set value o =
          Buffer.add_uint8 output_buf (sixbit_to_char (value land 0x3f));
          o + 1
        in
        let get i = Bytes.get_uint8 input i in
        let b1 = get i in
        let o = o |> set (b1 lsr 2) in (* b1[0]..b1[5] *)
        match len with
        | `I -> o |> set (b1 lsl 4) (* b1[6] b1[7] 0 0 0 0 *)
        | `S n ->
          let b2 = get (i+1) in
          let o = o |> set ((b1 lsl 4) lor (b2 lsr 4)) in (* b1[6] b1[7] b2[0]..b2[3] *)
          match n with
          | `I -> o |> set (b2 lsl 2) (* b2[4]..b2[7] 0 0*)
          | `S `I ->
            let b3 = get (i+2) in
            o |> set ((b2 lsl 2) lor (b3 lsr 6)) (* b2[4]..b2[7] b3[0] b3[1]*)
              |> set b3 (* b3[2]..b3[7] *)
      in
      let rec go i o =
        match input_end - i with
        | 0 ->
          begin match pad with
          | Some pad ->
            let pad_chars =
              match o mod 4 with
              | 0 -> 0 (* when len mod 3 = 0 *)
              | 2 -> 2 (* when len mod 3 = 1 *)
              | 3 -> 1 (* when len mod 3 = 2 *)
              | _ -> failwith "impossible"
            in
            List.range 0 pad_chars
            |> List.fold_left (fun o _ -> Buffer.add_char output_buf pad; o+1) o
          | None -> o
          end
        | 1 -> `I |> write i o |> go (i+1)
        | 2 -> `S `I |> write i o |> go (i+2)
        | _ -> `S (`S `I) |> write i o |> go (i+3)
      in
      let total_bytes = go input_offset 0 in
      Buffer.add_buffer output output_buf;
      total_bytes

    let encode ?offset ?len input =
      let output = Buffer.create 0 in
      encode_buf ?offset ?len output input |> ignore;
      Buffer.contents output

    let count_lenth_ignoring ?(offset=0) ?len (input: Bytes.t): int =
      let orig_len = Bytes.length input in
      let len = len |? (orig_len - offset) in
      let is_sixbit = char_to_sixbit &> Option.is_some in
      match ignore_unknown, ignore_newline with
      | true, _ ->
        let count acc i =
          let b = Bytes.get_uint8 input (offset + i) in
          if (is_sixbit b) then acc + 1 else acc
        in
        len |> iotafl count 0
      | false, true ->
        let count acc i =
          let b = Bytes.get_uint8 input (offset + i) in
          if (is_sixbit b) then acc + 1 else
          begin match char_of_int b with
          | '\r' | '\n' -> acc
          | _ -> acc + 1
          end
        in
        len |> iotafl count 0
      | false, false -> len

    let decode_buf ?(offset=0) ?len (output: Buffer.t) (input: string) =
      let input = Bytes.of_string input in
      let input_offset, input_end, input_length =
        let orig_len = Bytes.length input in
        let len = len |? (orig_len - offset) in
        let end_index = offset + len in
        if len < 0 || end_index > orig_len then
          invalid_arg' "Base64.encode: the input range (offset:%d, len:%d) is out of bounds" offset len
        else if Option.is_some pad then
          let actual_len = count_lenth_ignoring ~offset ~len input in
          if validate_padding && actual_len mod 4 <> 0 then
            invalid_arg "Base64.decode: wrong padding"
          else offset, end_index, len
        else offset, end_index, len
      in
      let output_buf =
        let estimated_bytes = (input_length / 4) * 3 + 2 in  (* worst case: 3n+2 bytes (= 4n+3 chars) *)
        Buffer.create estimated_bytes
      in
      let read stack o =
        let set o value = Buffer.add_uint8 output_buf (value land 0xff); o+1 in
        match List.rev stack with
        | [] -> o
        | [_] -> invalid_arg "Base64.decode: unterminated input"
        | s1 :: s2 :: ss ->
          let o = set o ((s1 lsl 2) lor (s2 lsr 4)) in (* s1[0]..s1[5] s2[0] s2[1] *)
          match ss with
          | [] -> (* [3n+1 bytes] 4bits = s2[2]..s2[5] should've been padded *)
            if not ((s2 land 0xf) = 0) then invalid_arg "Base64.decode: unterminated input"
            else o
          | s3 :: ss ->
            let o = set o ((s2 lsl 4) lor (s3 lsr 2)) in (* s2[2]..s1[5] s3[0]..[3] *)
            match ss with
            | [] -> (* [3n+2 bytes] 2bits = s3[4] s3[5] should've been padded *)
              if not ((s3 land 0x3) = 0) then invalid_arg "Base64.decode: unterminated input"
              else o
            | s4 :: [] -> (* [3n bytes] *)
              set o ((s3 lsl 6) lor s4) (* s3[4] s3[5] s4[0]..[5] *)
            | _ -> failwith "impossible"
      in
      let rec go stack i o =
        if i = input_end then read stack o
        else
          let c = Bytes.get_uint8 input i in
          match char_to_sixbit c with
          | Some s ->
            let stack = s :: stack in
            if List.length stack = 4 then
              let o = read stack o in
              go [] (i+1) o
            else
              go stack (i+1) o
          | None ->
            begin match char_of_int c with
            | _ when ignore_unknown -> go stack (i+1) o
            | '\r' | '\n' when ignore_newline -> go stack (i+1) o
            | c when pad = some c && not validate_padding -> read stack o
            | c when pad = some c && validate_padding ->
              let pad = c in
              let validate_pad ignore =
                let pad_count acc ci =
                  let c = Bytes.get_uint8 input (ci + i + 1) in
                  begin match char_of_int c with
                  | s when s = pad -> acc + 1
                  | s when ignore s -> acc
                  | s -> raise (Invalid_base64_padding (
                                    `invalid_char_with_position
                                      (s, ci + i + 1)))
                  end
                in
                let pad_num = (input_end - i - 1) |> iotafl pad_count 0 |> ((+) 1) in
                let is_valid = 0 < pad_num && pad_num <= 2 in
                if is_valid then read stack o
                else raise (Invalid_base64_padding (`invalid_padding_length pad_num))
              in
              begin match ignore_unknown, ignore_newline with
              | true, _ -> read stack o
              | false, true ->
                validate_pad begin function
                | '\r' | '\n' -> true
                | _ -> false
                end
              | false, false -> validate_pad (fun _ -> false)
              end
            | c -> invalid_arg' "Base64.decode: invalid char '%c' at index %d" c i
            end
      in
      let total_bytes = go [] input_offset 0 in
      Buffer.add_buffer output output_buf;
      total_bytes

    let decode ?offset ?len input =
      let output = Buffer.create 0 in
      decode_buf ?offset ?len output input |> ignore;
      Buffer.to_bytes output
  end

  module Config_rfc4648 : Config = struct
    let c62 = '+'
    let c63 = '/'
    let pad = Some '='
    let validate_padding = true
    let ignore_newline = false
    let ignore_unknown = false
  end
  module Config_rfc4648_relaxed = struct
    include Config_rfc4648
    let ignore_newline = true
  end
  include Make(Config_rfc4648_relaxed)

  module Config_rfc4648_url : Config = struct
    let c62 = '-'
    let c63 = '_'
    let pad = None
    let validate_padding = true
    let ignore_newline = false
    let ignore_unknown = false
  end
  module Config_rfc4648_url_relaxed = struct
    include Config_rfc4648_url
    let ignore_newline = true
  end
  module Url = Make(Config_rfc4648_url_relaxed)
end

(** encode / decode a string according to the RFC 3986 Section 2.1
    {i URI Generic Syntax - Percent-Encoding} syntax *)
module Url_encoding : sig

  (**
     Takes an input [bytes], and writes the encoded string to [Buffer.t].
     @param offset   the offset of input which the encoder should start reading from.
     @param len      the length of input which the encoder should read.
     @return the number of bytes written to [Buffer.t].
   *)
  val encode_buf : ?offset:int -> ?len:int -> Buffer.t -> bytes -> int (* bytes written *)

  (**
     Takes an input [string], and writes the decoded bytes to [Buffer.t].
     @param offset   the offset of input which the decoder should start reading from.
     @param len      the length of input which the decoder should read.
     @return the number of bytes written to [Buffer.t].
   *)
  val decode_buf : ?offset:int -> ?len:int -> Buffer.t -> string -> int (* bytes written *)

  (**
     Takes an input [bytes], and returns the encoded [string].
     @param offset   the offset of input which the encoder should start reading from.
     @param len      the length of input which the encoder should read.
   *)
  val encode : ?offset:int -> ?len:int -> bytes -> string

  (**
     Takes an input [string], and returns the decoded [bytes].
     @param offset   the offset of input which the decoder should start reading from.
     @param len      the length of input which the decoder should read.
   *)
  val decode : ?offset:int -> ?len:int -> string -> bytes

  exception Invalid_url_encoded_string of [
    | `reserved_character_in_encoded_string of char * int
    | `invalid_percent_encoded_character of string * int
    | `percent_encoded_character_end_of_input of int ]

end = struct

  exception Invalid_url_encoded_string of [
    | `reserved_character_in_encoded_string of char * int
    | `invalid_percent_encoded_character of string * int
    | `percent_encoded_character_end_of_input of int ]
  let () =
    Printexc.register_printer (function
        | Invalid_url_encoded_string (
            `reserved_character_in_encoded_string (c, p))
          -> sprintf "Invalid_url_encoded_string
                      (reserved character %C at %d)"
               c p |> some
        | Invalid_url_encoded_string (
            `invalid_percent_encoded_character (s, p))
          -> sprintf "Invalid_url_encoded_string
                      (invalid percent encoding %S at %d)"
               s p |> some
        | Invalid_url_encoded_string (
            `percent_encoded_character_end_of_input p)
          -> sprintf "Invalid_url_encoded_string\
                      ('%%' too close to the end-of-input at %d)"
               p |> some
        | _ -> none)

  let safe_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
      | '-' | '.' | '_' | '~' -> true
    | _ -> false

  let encoding_table =
    lazy (iotaf (sprintf "%%%02X") 256  |> List.to_function)

  let decoding_table =
    lazy (
      let table1 = Lazy.force encoding_table in
      let table2 = Seq.iotaf (?+< (sprintf "%%%02x")) 256 in
      iotaf (?+< table1) 256
      |> List.to_seq
      |> Seq.append table2
      |> StringMap.of_seq)

  let encode_buf ?(offset = 0) ?len buf data =
    let table = int_of_char &> Lazy.force encoding_table in
    let dlen = Bytes.length data in
    let olen = len |? dlen - offset in
    if olen < 0 || offset + olen > dlen then (
      invalid_arg' "Url_encoding.encode: the input range (offset:%d, len:%d) is out of bounds" offset olen;
    );
    let rec loop p = function
      | 0 -> olen
      | n ->
         let c = Bytes.get data p in
         (if safe_char c
          then Buffer.add_char buf c
          else Buffer.add_string buf (table c)
         );
         loop (succ p) (pred n)
    in
    loop offset olen

  let decode_buf ?(offset = 0) ?len buf data =
    let lookup =
      let table = Lazy.force decoding_table in
      fun s -> StringMap.find_opt s table in
    let dlen = String.length data in
    let ilen = len |? dlen - offset in
    if ilen < 0 || offset + ilen > dlen then (
      invalid_arg' "Url_encoding.decode: the input range (offset:%d, len:%d) is out of bounds" offset ilen;
    );
    let rec loop p = function
      | 0 -> ilen
      | n -> (
        match String.get data p with
        | '%' when n < 3 ->
           raise (Invalid_url_encoded_string (`percent_encoded_character_end_of_input p))
        | '%' ->
           let s = String.sub data p 3 in
           lookup s
           |> (Option.or_raise (
                   Invalid_url_encoded_string (
                       `invalid_percent_encoded_character (s, p))))
           |> Buffer.add_uint8 buf;
           loop (p + 3) (n - 3)
        | c when safe_char c ->
           Buffer.add_char buf c;
           loop (succ p) (pred n)
        | c ->
           raise (Invalid_url_encoded_string (
                      `reserved_character_in_encoded_string (c, p))))
    in
    loop offset ilen

    let encode ?offset ?len input =
      let output = Buffer.create 0 in
      encode_buf ?offset ?len output input |> ignore;
      Buffer.contents output

    let decode ?offset ?len input =
      let output = Buffer.create 0 in
      decode_buf ?offset ?len output input |> ignore;
      Buffer.to_bytes output
end
