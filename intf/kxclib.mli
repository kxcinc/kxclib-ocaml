
(** {2} Utilities regarding refs *)

(** [refset r x] sets [x] to ref [r]. *)
val refset : 'a ref -> 'a -> unit

(** [refget r] returns [!r]. *)
val refget : 'a ref -> 'a

(** [refupdate r f] updates referent of [r] by [f]. *)
val refupdate : 'a ref -> ('a -> 'a) -> unit

(** [refupdate' f r] is equivalent to [refupdate r f]. *)
val refupdate' : ('a -> 'a) -> 'a ref -> unit

(** [refupdate_and_calc r f]
    calculate a result and the a updated referent value
    from the current referent value of [r] using [f]. *)
val refupdate_and_calc : 'x ref -> ('x -> 'a * 'x) -> 'a

(** [refappend r x] appends [x] to referent of [r]. *)
val refappend : 'a list ref -> 'a -> unit

(** [refappend' x r] is equivalent to [refappend r x]. *)
val refappend' : 'a -> 'a list ref -> unit

(** [refpop r] pop first item of the list referred to by [r].
    {b Raises} [Not_found] if the list is empty. *)
val refpop : 'a list ref -> 'a

(** [incr r] increases the referent of [r] by one. *)
val incr : int ref -> unit

(** [decr r] decreases the referent of [r] by one. *)
val decr : int ref -> unit

val refupdate'_and_get : ('a -> 'a) -> 'a ref -> 'a
val get_and_refupdate' : ('a -> 'a) -> 'a ref -> 'a
val incr_and_get : int ref -> int
val decr_and_get : int ref -> int
val get_and_incr : int ref -> int
val get_and_decr : int ref -> int

(** {2} Exception helper functions *)

val failwith' : ('a, Format.formatter, unit, 'b) format4 -> 'a
val invalid_arg' : ('a, Format.formatter, unit, 'b) format4 -> 'a

(** {2} (Section name todo) *)

val neg : int -> int
val mul : int -> int -> int
val div : int -> int -> int
val rem : int -> int -> int

(** {2} Utilities useful in idiomatic functional programming *)

(** constant function *)
val constant : 'a -> 'b -> 'a

(** identity function *)
val identity : 'a -> 'a

val iotaf : (int -> 'a) -> int -> 'a list
val iotaf' : (int -> unit) -> int -> unit
val iotafl : ('a -> int -> 'a) -> 'a -> int -> 'a
val iotafl' : ('a -> 'b -> 'a) -> 'a -> (int -> 'b) -> int -> 'a

val min_by : ('a -> 'b) -> 'a -> 'a -> 'a
val max_by : ('a -> 'b) -> 'a -> 'a -> 'a

val projected_compare : ?cmp:('b -> 'b -> int) -> ('a -> 'b) -> 'a -> 'a -> int

val dup : 'x -> 'x * 'x
val swap : 'x * 'y -> 'y * 'x

val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b

module Functionals :
sig
  val negate : ('a -> bool) -> 'a -> bool
  val both : ('a -> bool) -> ('a -> bool) -> 'a -> bool
  val either : ('a -> bool) -> ('a -> bool) -> 'a -> bool
  val dig2nd : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  val dig3rd : ('a -> 'b -> 'c -> 'd) -> 'c -> 'a -> 'b -> 'd
  val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  val fix1st : 'a -> ('a -> 'b) -> 'b
  val fix2nd : 'a -> ('b -> 'a -> 'c) -> 'b -> 'c
  val fix3rd : 'a -> ('b -> 'c -> 'a -> 'd) -> 'b -> 'c -> 'd
  val fix1st' : 'a -> ('a -> 'b) -> 'c -> 'b
  val tap : ('a -> unit) -> 'a -> 'a
  val reptill : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
  val ntimes : int -> ('a -> 'a) -> 'a -> 'a
  val dotill : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
  val fixpoint : ?maxn:int -> ('a -> 'a) -> 'a -> 'a
  val converge' :
    (int -> 'a -> 'a -> bool) -> ('a -> 'a) -> 'a -> ('a, 'b) result
  val converge :
    ('a -> 'a -> bool) -> ('a -> 'a) -> 'a -> ('a, 'b) result
  module BasicInfix :
  sig
    val ( & ) : ('x -> 'y) -> 'x -> 'y
    val ( % ) : ('y -> 'z) -> ('x -> 'y) -> 'x -> 'z
    val ( %% ) : ('a -> 'y -> 'z) -> ('x -> 'y) -> 'a -> 'x -> 'z
    val ( &> ) : ('x -> 'y) -> ('y -> 'z) -> 'x -> 'z
    val ( ?. ) : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
    val ( ?.. ) : ('a -> 'b -> 'c -> 'd) -> 'c -> 'a -> 'b -> 'd
    val ( !. ) : 'b -> ('b -> 'b -> 'c) -> 'b -> 'c
    val ( !.. ) : 'c -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'd
    val ( &&> ) : ('x -> 'y -> 'z) -> ('z -> 'r) -> 'x -> 'y -> 'r
    val ( |-> ) : 'x -> ('x -> unit) -> 'x
    val ( // ) : ('a -> 'x) -> ('b -> 'y) -> 'a * 'b -> 'x * 'y
    val ( /> ) : 'a * 'b -> ('b -> 'c) -> 'a * 'c
    val ( /< ) : 'a * 'b -> ('a -> 'c) -> 'c * 'b
    val ( |+> ) : 'a -> ('a -> 'b) -> 'a * 'b
    val ( |+< ) : 'a -> ('a -> 'b) -> 'b * 'a
    val ( ?> ) : ('b -> 'c) -> 'a * 'b -> 'a * 'c
    val ( ?< ) : ('a -> 'c) -> 'a * 'b -> 'c * 'b
    val ( ?+> ) : ('a -> 'b) -> 'a -> 'a * 'b
    val ( ?+< ) : ('a -> 'b) -> 'a -> 'b * 'a
    val ( ?&> ) : ('y2 -> 'x2) -> ('x1 * 'x2 -> 'r) -> 'x1 * 'y2 -> 'r
    val ( ?&< ) : ('y1 -> 'x1) -> ('x1 * 'x2 -> 'r) -> 'y1 * 'x2 -> 'r
    val ( !! ) : ('a -> 'b -> 'x) -> 'a * 'b -> 'x
    val ( !? ) : ('a * 'b -> 'x) -> 'a -> 'b -> 'x
  end
  module CommonTypes : sig
    type null = |
    type 'x endo = 'x -> 'x
  end
  module Infix = BasicInfix
end

module Fn = Functionals

include module type of Functionals.BasicInfix
include module type of Functionals.CommonTypes

(** {2} (Section name todo) *)

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

module PipeOps : functor (S : Pipeable) -> PipeOpsS with type 'x pipeable := 'x S.t

module PipeOps_flat_map : functor (S : Pipeable_flat_map) -> PipeOpsS with type 'x pipeable := 'x S.t

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

  (** [m >>= do_cond cond f_true f_false] performs [f_true] or [f_false]
      on value enclosed in [m],
      respectively when [cond] is [true] or [false];

      functionally equiv. to [fun c f1 f2 -> if c then f1 else f2] but
      return type of [f1] and [f2] is restricted to [_ t] *)
  val do_cond : bool -> ('a -> 'b t) -> ('a -> 'b t) -> 'a -> 'b t

  (** [m >>= do_if cond f] is equiv. to [m >>= do_cond cond f (returning ())] *)
  val do_if : bool -> ('a -> unit t) -> 'a -> unit t

  val sequence_list : 'a t list -> 'a list t

  (** monadic binding version of {!sequence_list} *)
  val ( >>=* ) : 'x t list -> ('x list -> 'y t) -> 'y t
end

module MonadOps : functor (M : Monadic) -> MonadOpsS with type 'x t := 'x M.t

(** {2} Enhanced Stdlib *)

module Either : sig
  [%%if ocaml_version < (4, 12, 0)]
  type ('a, 'b) t = Left of 'a | Right of 'b
  [%%else]
  include module type of Stdlib.Either
  [%%endif]

  val left : 'a -> ('a, 'b) t
  val right : 'a -> ('b, 'a) t
end
type ('a, 'b) either = ('a, 'b) Either.t

module Result : sig
  include module type of Stdlib.Result
  val concat : ('x, 'e) result list -> ('x list, 'e) result
end

module type ResultOfS = sig
  type err
  exception Error_result of err

  include Monadic with type 'x t = ('x, err) result
  val error : err -> 'a t
end

module ResultOf : functor (E : sig type err end) -> ResultOfS with type err = E.err

module type ResultOfS' = sig
  include ResultOfS
  val get : 'x t -> 'x
end

module ResultOf' : functor
  (E : sig
     type err
     val string_of_err : err -> string option
   end
  ) -> ResultOfS' with type err = E.err

module ResultWithErrmsg0 : sig
  include ResultOfS'
  val protect' : handler:(exn -> string) -> ('x -> 'y) -> 'x -> 'y t
  val protect : ('x -> 'y) -> 'x -> 'y t
end

module Queue : sig
  type 'x t
  val empty : 'x t
  val is_empty : 'x t -> bool
  val push : 'x -> 'x t -> 'x t
  val push_front : 'x -> 'x t -> 'x t
  val pop : 'x t -> ('x * 'x t) option
  val peek : 'x t -> ('x * 'x t) option
end
type 'x queue = 'x Queue.t

module Option : sig
  include module type of Stdlib.Option
  include Monadic with type 'x t := 'x t

  module Ops_monad : MonadOpsS with type 'x t := 'x t

  (* TODO *)
  (* module Ops_piping : PipeOpsS with type 'x pipeable := 'x t *)

  module Ops : sig
    include module type of Ops_monad

    (* TODO *)
    (* include module type of Ops_piping *)
  end

  val get : 'a t -> 'a
  val v : 'a -> 'a t -> 'a
  val v' : (unit -> 'a) -> 'a t -> 'a
  val or_raise : exn -> 'a t -> 'a
  val otherwise : 'a t -> 'a t -> 'a t
  val otherwise' : (unit -> 'a t) -> 'a t -> 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val filter : ('a -> bool) -> 'a t -> 'a t

  val fmap : ('a -> 'b t t) -> 'a t -> 'b t

  val of_bool : bool -> unit t
  val some_if : bool -> 'a -> 'a t

  (** [protect ~capture f x] returns [Some (f x)] except when
      - [f x] throws an [exn] s.t. [capture exn = true], it returns [None]
      - [f x] throws an [exn] s.t. [capture exn = false], it rethrows [exn]

      [~capture] defaults to [fun _exn -> true] *)
  val protect : ?capture:(exn -> bool) -> ('x -> 'y) -> 'x -> 'y option

  (** [try_make ~capture f] is [protect ~capture f ()], thus see {!protect}

      [~capture] defaults to [fun _exn -> true] *)
  val try_make : ?capture:(exn -> bool) -> (unit -> 'x) -> 'x option

  (** a specialized version of [try_make] where [~capture] is fixed to
      [function Not_found -> true | _ -> false] *)
  val if_found : (unit -> 'x) -> 'x option

end

val some : 'a -> 'a option
val none : 'a option

val ( >? ) : 'a option -> ('a -> 'b) -> 'b option
val ( >>? ) : 'a option -> ('a -> 'b option) -> 'b option
val ( |? ) : 'a Option.t -> 'a -> 'a
val ( |?! ) : 'a Option.t -> (unit -> 'a) -> 'a
val ( ||? ) : 'a Option.t -> 'a Option.t -> 'a Option.t
val ( ||?! ) : 'a Option.t -> (unit -> 'a Option.t) -> 'a Option.t
val ( &>? ) : ('x -> 'y option) -> ('y -> 'z) -> 'x -> 'z option
val ( &>>? ) : ('x -> 'y option) -> ('y -> 'z option) -> 'x -> 'z option

module Seq : sig
  include module type of Stdlib.Seq
  include Monadic with type 'x t := 'x t

  module Ops_monad : MonadOpsS with type 'x t := 'x t
  module Ops_piping : PipeOpsS with type 'x pipeable := 'x t

  module Ops : sig
    include module type of Ops_monad
    include module type of Ops_piping
  end

  val from : (unit -> 'x option) -> 'x t

  val iota : int -> int t
  val iotaf : (int -> 'a) -> int -> 'a Seq.t
  val iotaf' : (int -> unit) -> int -> unit

  val length : 'a t -> int

  val range : ?include_endpoint:bool -> int -> int -> int Seq.t

  val enum : int -> int t
  val limited : int -> 'a Seq.t -> 'a Seq.t
  val iteri : (int -> 'a -> unit) -> (unit -> 'a node) -> unit
  val hd : (unit -> 'a node) -> 'a
  val tl : (unit -> 'a node) -> 'a t
  val take : int -> 'a t -> 'a t
  val drop : int -> 'a t -> 'a t
  val make : int -> 'a -> 'a t
end
type 'x seq = 'x Seq.t

module Array : sig
  include module type of Stdlib.Array
  include Monadic with type 'x t := 'x t

  module Ops_monad : MonadOpsS with type 'x t := 'x t
  module Ops_piping : PipeOpsS with type 'x pipeable := 'x t

  module Ops : sig
    include module type of Ops_monad
    include module type of Ops_piping
  end

  val of_list_of_length : int -> 'a list -> 'a array
  val mean : ?f:(float -> float) -> float t -> float
  val min : ('a -> 'a -> int) -> 'a array -> 'a
  val max : ('a -> 'a -> int) -> 'a array -> 'a
  val first : 'a array -> 'a
  val last : 'a array -> 'a
  val sorted : ('a -> 'a -> int) -> 'a array -> 'a array
  val update : ('a -> 'a) -> 'a array -> int -> unit
  val update_each : (int -> 'a -> 'a) -> 'a array -> unit
  val blastsati : ('a -> bool) -> 'a array -> int
  val blastsat : ('a -> bool) -> 'a array -> 'a
  val swap : 'a array -> int -> int -> unit
  val shuffle : ?rng:(int -> int) -> 'x array -> unit
  val to_function : 'a array -> int -> 'a
end

[%%if ocaml_version < (4, 14, 0)]
module Stream : sig
  include module type of Stream

  val to_list_rev : 'a -> 'b list
  val to_list : 'a -> 'b list
  val hd : 'a -> 'b
  val drop1 : 'a -> 'a
  val take : int -> 'a -> 'b
  val drop : int -> 'a -> 'a
end
[%%endif]

module List : sig
  include module type of Stdlib.List
  include Monadic with type 'x t := 'x t

  module Ops_monad : MonadOpsS with type 'x t := 'x t
  module Ops_piping : PipeOpsS with type 'x pipeable := 'x t

  module Ops : sig
    include module type of Ops_monad
    include module type of Ops_piping
  end

  val conj : 'x -> 'x list -> 'x list
  (** [conj x xs] append [x] as the last element of [xs] *)

  val conj_rev : 'x -> 'x list -> 'x list
  (** [conj_rev x xs'] is the equiv of [conj x (List.rev xs')] *)

  val iota : int -> int t
  val iota1 : int -> int t
  val range : ?include_endpoint:bool -> int -> int -> int List.t

  val dedup' : by:('a -> 'b) -> 'a List.t -> 'a List.t
  val dedup : 'a List.t -> 'a List.t

  val update_assoc :
    'k -> ('v option -> 'v option) -> ('k * 'v) list -> ('k * 'v) list
  val update_assq :
    'k -> ('v option -> 'v option) -> ('k * 'v) list -> ('k * 'v) list

  (** [deassoc_opt k l] removes entry keyed [k] from [l], interpreted as an association list,
      and return [v, l'] where [v] is the value of the entry being removed or [None], and
      [l'] is the list after the removal, or semantically unchanged if the key does not exist.
      note that entries in [l'] may differ in order wrt. [l].

      if there are multiple entries keyed [k], [v] will be [Some _] and [l'] will differ from the
      original, but otherwise the behavior is unspecified *)
  val deassoc_opt : 'k -> ('k * 'v) list -> 'v option * ('k * 'v) list

  (** same as {!deassoc_opt} but different return type *)
  val deassoc_opt' : 'k -> ('k * 'v) list -> ('v * ('k * 'v) list) option

  (** same as {!deassoc_opt} except using [(==)] when comparing keys *)
  val deassq_opt : 'k -> ('k * 'v) list -> 'v option * ('k * 'v) list

  (** same as {!deassq_opt} but different return type *)
  val deassq_opt' : 'k -> ('k * 'v) list -> ('v * ('k * 'v) list) option

  val deassoc : 'k -> ('k * 'v) list -> 'v * ('k * 'v) list
  (** same as {!deassoc_opt} but raises [Not_found] when the requested key does not exist *)

  val deassq : 'k -> ('k * 'v) list -> 'v * ('k * 'v) list
  (** same as {!deassq_opt} but raises [Not_found] when the requested key does not exist *)

  val group_by : ('x -> 'k) -> 'x t -> ('k * 'x t) t
  val unzip : ('a * 'b) list -> 'a t * 'b t
  val unzip3 : ('a * 'b * 'c) list -> 'a t * 'b t * 'c t

  (** raises [Not_found] if empty list; see also {!reduce_opt} *)
  val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a

  (** return [None] if empty list; see also {!reduce} *)
  val reduce_opt : ('a -> 'a -> 'a) -> 'a t -> 'a option

  val min_opt : ('a -> 'a -> int) -> 'a t -> 'a option
  val max_opt : ('a -> 'a -> int) -> 'a t -> 'a option
  val min : ('a -> 'a -> int) -> 'a t -> 'a
  val max : ('a -> 'a -> int) -> 'a t -> 'a

  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val make : int -> 'a -> 'a list

  (** raises [Not_found] if empty list *)
  val hd : 'a t -> 'a

  (** raises [Not_found] if empty list *)
  val tl : 'a t -> 'a list

  (** raises [Not_found] if empty list *)
  val take : int -> 'a t -> 'a list

  (** raises [Not_found] if empty list *)
  val drop : int -> 'a t -> 'a t

  (** [pred list] returns the number of elements [e] in [list] that satisfies [pred] *)
  val count : ('a -> bool) -> 'a list -> int

  (** last element of list; raises [Not_found] if empty list *)
  val last : 'a list -> 'a

  (** last element and rest of a list; raises [Not_found] if empty list *)
  val and_last : 'x list -> 'x list * 'x

  val iter' : ('a -> unit) -> ('a -> unit) -> 'a t -> unit

  val fmap : ('x -> 'y list) -> 'x list -> 'y list
  val interpolate : 'a -> 'a t -> 'a list
  val filteri : (int -> 'a -> bool) -> 'a t -> 'a list

  val empty : 'a t -> bool

  val to_function : 'a list -> int -> 'a
  val to_hashtbl : ('k * 'v) list -> ('k, 'v) Hashtbl.t

  val pp :
    ?sep:string ->
    ?parens:string * string ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a list -> unit
end

include module type of List.Ops_piping

val iota : int -> int List.t
val iota1 : int -> int List.t

module Hashtbl : sig
  include module type of Stdlib.Hashtbl

  val to_function : ('a, 'b) t -> 'a -> 'b

  (** swap the key and value *)
  val rev : ('a, 'b) t -> ('b, 'a) t

  (** [make n genfunc] creates a hashtable of [n] elements with entries
      [{ (fst (genfunc 0))  |-> (snd (genfunc 0))
       , (fst (genfunc 1))  |-> (snd (genfunc 1))
         ...
       , (fst (genfunc (n-1)))  |-> (snd (genfunc (n-1)))
       }]  *)
  val make :
    ?random:bool -> int -> (int -> 'a * 'b) -> ('a, 'b) Hashtbl.t
end

module Bytes : sig
  include module type of Stdlib.Bytes
end

module String : sig
  include module type of String

  [%%if ocaml_version < (4, 13, 0)]
  val exists : (char -> bool) -> string -> bool
  val for_all : (char -> bool) -> string -> bool
  [%%endif]

  (** [partition_opt n s] returns [None] if [n] is greater than the length of [s] or
      [Some (s1, s2)] where [s = s1^s2 && n = length s1] otherwise *)
  val partition_opt : int -> string -> (string * string) option

  (** [partition n s] returns [Some (s1, s2)] where [s = s1^s2 && n = length s1].

      {!Invalid_argument} will be thrown if [n] is greater than the length of [s] *)
  val partition : int -> string -> string * string

  (** [empty str] returns true when str is of zero length *)
  val empty : string -> bool

  (** [empty_trimmed str] returns true when str is of zero length after being trimmed *)
  val empty_trimmed : string -> bool

  (** [chop_prefix p s] returns [s] minus the prefix [p] wrapped in [Some],
      or [None] if [s] does not start with [p] *)
  val chop_prefix : string -> string -> string option

  (** [starts_with p s] returns whether [s] starts with a substring of [p] *)
  val starts_with : string -> string -> bool

  (** [ends_with p s] returns whether [s] ends with a substring of [p] *)
  val ends_with : string -> string -> bool

  (** [chop_prefix p s] returns [s] minus the suffix [p] wrapped in [Some],
      or [None] if [s] does not end with [p] *)
  val chop_suffix : string -> string -> string option
  val to_bytes : string -> bytes
  val to_list : t -> char list
  val of_list : char list -> t
  val of_array : char array -> t

  val valid_ascii_js_identifier : string -> bool
  val pp_json_escaped : Format.formatter -> string -> unit
  val json_escaped : string -> string
end

module MapPlus : functor (M : Map.S) -> sig
  val pp' :
    (Format.formatter -> M.key -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a M.t -> unit
  val of_list : (M.key * 'v) list -> 'v M.t
end

module StringMap : sig
  include module type of Map.Make(String)
  include module type of MapPlus(Map.Make(String))
  val pp :
    (Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a t
    -> unit
end

module IntMap : sig
  include module type of Map.Make(Int)
  include module type of MapPlus(Map.Make(Int))
  val pp :
    (Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a t
    -> unit
end

module Obj : sig
  include module type of Obj

  [%%if ocaml_version < (5, 4, 0)]
  val hash_variant : string -> int
  [%%else]
  val hash_variant : string -> int
  [@@alert unavailable "hash_variant is not available for this OCaml version"]
  [%%endif]
end

module IoPervasives : sig
  val with_input_file : string -> (in_channel -> 'a) -> 'a
  val with_output_file : string -> (out_channel -> 'a) -> 'a
  val slurp_input : ?buf:bytes -> in_channel -> string
  val slurp_stdin : ?buf:bytes -> unit -> string
  val slurp_file : string -> string
  val spit_file : string -> string -> unit
end

include module type of IoPervasives

module Timing : sig

  (** time the execution of [f], returning the result
        of [f] and store the measured time in [output] *)
  val timefunc' : float ref -> (unit -> 'a) -> 'a

  (** time the execution of [f], discarding the result of [f] *)
  val timefunc : (unit -> 'a) -> float
end

module Int53p : sig
  type int53p_impl_flavor =
    [ `custom_impl of string | `float_impl | `int64_impl | `int_impl ]
  val pp_int53p_impl_flavor :
    Format.formatter -> int53p_impl_flavor -> unit
  val show_int53p_impl_flavor : int53p_impl_flavor -> string

  module type Ops = sig
    type int53p
    val ( ~-% ) : int53p -> int53p
    val ( ~+% ) : int53p -> int53p
    val ( +% ) : int53p -> int53p -> int53p
    val ( -% ) : int53p -> int53p -> int53p
    val ( *% ) : int53p -> int53p -> int53p
    val ( /% ) : int53p -> int53p -> int53p
    val ( /%% ) : int53p -> int53p -> int53p
  end

  module type Prims = sig
    type int53p
    val neg : int53p -> int53p
    val add : int53p -> int53p -> int53p
    val sub : int53p -> int53p -> int53p
    val mul : int53p -> int53p -> int53p
    val div : int53p -> int53p -> int53p
    val rem : int53p -> int53p -> int53p
  end

  module type S = sig
    val impl_flavor : int53p_impl_flavor

    type int53p

    module Ops : Ops with type int53p = int53p

    include module type of Ops with type int53p := int53p
    include Prims with type int53p := int53p

    type t = int53p

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
  module Internals : sig
    module MakeOps : functor (P : Prims) -> Ops with type int53p = P.int53p
    module IntImpl : S
    module Int64Impl : S
    module FloatImpl : S
    val current_impl_flavor :
      [> `float_impl | `int64_impl | `int_impl ]
    val impl_of_builtin_flavor : int53p_impl_flavor -> (module S)
    module CurrentFlavorImpl : S
  end

  include module type of Internals.CurrentFlavorImpl

  val pp_int53p : Format.formatter -> int53p -> unit
  val show_int53p : int53p -> string

  type t = int53p

  val pp: Format.formatter -> t -> unit
  val show: t -> string
end
include module type of Int53p.Ops

val pp_int53p : Format.formatter -> int53p -> unit
val show_int53p : int53p -> string

module Datetime0 : sig

  (** all according to proleptic Gregorian Calender *)

  val leap_year : int -> bool
  val daycount_of_month : leap:bool -> int -> int
  val day_of_year : int -> int -> int -> int

  module type NormalizedTimestamp = sig

    (** timezone not taking into consideration *)

    module Conf : sig

      (** the epoch would be at January 1st 00:00:00.0 in [epoch_year] *)
      val epoch_year : int

      (** e.g. sec-resolution use [1] and millisec-resolution use [1000] *)
      val subsecond_resolution : int

      (** min-year supported *)
      val min_year : int

      (** max-year supported *)
      val max_year : int

    end

    (** [normalize
         ?tzoffset:(tzhour, tzmin)
         ?subsec (yy, mm, dd) (hour, min, sec)]
        calculates the normalized timestamp *)
    val normalize :
      ?subsec:int ->
      ?tzoffset:int * int -> int * int * int -> int * int * int -> int

  end

  module EpochNormalizedTimestamp : functor
    (_ : sig
       val epoch_year : int
       val subsecond_resolution : int
     end
    ) -> NormalizedTimestamp

  module UnixTimestmapSecRes : NormalizedTimestamp
  module UnixTimestmapMilliRes : NormalizedTimestamp
  module UnixTimestmapNanoRes : NormalizedTimestamp
end

module ParseArgs : sig
  type optparser = string -> [ `Process_next of bool ]
  val prefset : 'a ref -> 'a -> unit
  val prefsetv : 'a ref -> 'a -> 'b -> unit
  val scanfparser :
    ('a, Scanf.Scanning.in_channel, 'b, 'c -> unit,
     'a -> ([> `Process_next of bool ] as 'd), 'd
    ) format6 -> 'c -> optparser
  val exactparser : string -> (unit -> unit) -> optparser
  val parse_opts :
    optparser list -> ?argsource:string array * int -> unit -> unit
  val parse_opts_args :
    ?optprefix:string ->
    ?optsep:string ->
    optparser list ->
    ?argsource:string array * int -> unit -> string list
end
module ArgOptions :
sig
  type _ named_option =
    IntOption : string -> int named_option
  | FloatOption : string -> float named_option
  | StringOption : string -> string named_option
  | InChannelOption : string -> in_channel named_option
  | OutChannelOption : string -> out_channel named_option
  | InChannelOption' :
      string -> (in_channel * channel_desc) named_option
  | OutChannelOption' :
      string -> (out_channel * channel_desc) named_option
  and channel_desc = [ `FileChannel of string | `StandardChannel ]
  val opt_of_named_option : 'x named_option -> string
  module type FeatureRequests =
    sig
      val has_flag :
        ?argsource:string array * int -> ?prefix:string -> string -> bool
      val get_option :
        ?argsource:string array * int ->
        ?optprefix:string -> ?optsep:string -> 'x named_option -> 'x
      val get_option_d :
        ?argsource:string array * int ->
        ?optprefix:string ->
        ?optsep:string -> 'x named_option -> 'x  (** default value *) -> 'x
      val get_option_d' :
        ?argsource:string array * int ->
        ?optprefix:string ->
        ?optsep:string -> 'x named_option -> (unit -> 'x) (** default value producer *) -> 'x
      val get_args :
        ?argsource:string array * int ->
        ?optsep:string -> unit -> string list
    end
  val has_flag :
    ?argsource:string array * int -> ?prefix:string -> string -> bool
  val get_option :
    ?argsource:string array * int ->
    ?prefix:string -> ?optsep:string -> 'x named_option -> 'x option
  val get_option_exn :
    ?argsource:string array * int ->
    ?prefix:string -> ?optsep:string -> 'x named_option -> 'x
  val get_option_d' :
    ?argsource:string array * int ->
    ?prefix:string ->
    ?optsep:string -> 'x named_option -> (unit -> 'x) -> 'x
  val get_option_d :
    ?argsource:string array * int ->
    ?prefix:string -> ?optsep:string -> 'x named_option -> 'x -> 'x
  val get_absolute_args :
    ?optsep:string ->
    ?argsource:string array * int -> unit -> string list
end
module FmtPervasives :
sig
  type ppf = Format.formatter
  val color_enabled : bool ref
  val fprintf :
    Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  val printf : ('a, Format.formatter, unit) format -> 'a
  val sprintf : ('a, Format.formatter, unit, string) format4 -> 'a
  val eprintf : ('a, Format.formatter, unit) format -> 'a
  module Fmt :
  sig
    val stdout_ppf : Format.formatter
    val stderr_ppf : Format.formatter
    val null_ppf : Format.formatter
    val colored :
      ?style:[< `Bold | `Italic | `Thin | `Underline ] ->
      ?color_mode:[< `Bg | `Fg > `Fg ] ->
      [< `Black
      | `Blue
      | `Bright_black
      | `Bright_blue
      | `Bright_cyan
      | `Bright_green
      | `Bright_magenta
      | `Bright_red
      | `Bright_yellow
      | `Cyan
      | `Green
      | `Magenta
      | `Red
      | `White
      | `Yellow ] ->
      Format.formatter ->
      ('a, Format.formatter, unit, unit) format4 -> 'a
  end
  val condformat :
    bool ->
    (('a, Format.formatter, unit) format -> 'a) ->
    ('a, Format.formatter, unit) format -> 'a
  val pp_of_to_string : ('a -> string) -> Format.formatter -> 'a -> unit
  val to_string_of_pp : (Format.formatter -> 'a -> unit) -> 'a -> string
  val pps : ('a -> string) -> Format.formatter -> 'a -> unit
  val spp : (Format.formatter -> 'a -> unit) -> 'a -> string
  val pp_int : Format.formatter -> int -> unit
  val pp_float : Format.formatter -> float -> unit
  val pp_string : Format.formatter -> string -> unit
  val pp_string_quoted : Format.formatter -> string -> unit
  val pp_char : Format.formatter -> char -> unit
  val pp_bool : Format.formatter -> bool -> unit
  val pp_unit : Format.formatter -> unit -> unit
  val pp_ref_address : Format.formatter -> 'x ref -> unit
  val pp_int32 : Format.formatter -> int32 -> unit
  val pp_int64 : Format.formatter -> int64 -> unit
  val pp_integer_sep' :
    padding:(int * char) option -> Format.formatter -> int -> unit
  val pp_integer_sep : Format.formatter -> int -> unit
  val pp_multiline : Format.formatter -> string -> unit
  val pp_exn : Format.formatter -> exn -> unit
  val pp_full_exn' :
    Format.formatter -> exn * Printexc.raw_backtrace -> unit
  val pp_full_exn : Format.formatter -> exn -> unit
  val string_of_symbolic_output_items :
    Format.symbolic_output_item list -> string
end
type ppf = Format.formatter
val color_enabled : bool ref
val fprintf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
val printf : ('a, Format.formatter, unit) format -> 'a
val sprintf : ('a, Format.formatter, unit, string) format4 -> 'a
val eprintf : ('a, Format.formatter, unit) format -> 'a
module Fmt = FmtPervasives.Fmt
val condformat :
  bool ->
  (('a, Format.formatter, unit) format -> 'a) ->
  ('a, Format.formatter, unit) format -> 'a
val pp_of_to_string : ('a -> string) -> Format.formatter -> 'a -> unit
val to_string_of_pp : (Format.formatter -> 'a -> unit) -> 'a -> string
val pps : ('a -> string) -> Format.formatter -> 'a -> unit
val spp : (Format.formatter -> 'a -> unit) -> 'a -> string
val pp_int : Format.formatter -> int -> unit
val pp_float : Format.formatter -> float -> unit
val pp_string : Format.formatter -> string -> unit
val pp_string_quoted : Format.formatter -> string -> unit
val pp_char : Format.formatter -> char -> unit
val pp_bool : Format.formatter -> bool -> unit
val pp_unit : Format.formatter -> unit -> unit
val pp_ref_address : Format.formatter -> 'x ref -> unit
val pp_int32 : Format.formatter -> int32 -> unit
val pp_int64 : Format.formatter -> int64 -> unit
val pp_integer_sep' :
  padding:(int * char) option -> Format.formatter -> int -> unit
val pp_integer_sep : Format.formatter -> int -> unit
val pp_multiline : Format.formatter -> string -> unit
val pp_exn : Format.formatter -> exn -> unit
val pp_full_exn' : Format.formatter -> exn * Printexc.raw_backtrace -> unit
val pp_full_exn : Format.formatter -> exn -> unit
val string_of_symbolic_output_items :
  Format.symbolic_output_item list -> string
module Log0 :
sig
  type log_filter =
    | LogFilter_by_label_whitelist of string list
    | LogFilter_by_label_blacklist of string list

  val pp_log_filter : Format.formatter -> log_filter -> unit
  val string_of_log_filter : log_filter -> string

  module Internals :
  sig
    val timestamp_func : (unit -> float option) ref
    val logging_formatter : Format.formatter ref
    val log_filter : log_filter option ref
  end
  module LoggingConfig :
  sig
    val install_timestamp_function : (unit -> float option) -> unit
    val set_logging_formatter : Format.formatter -> unit
    val get_logging_formatter : unit -> Format.formatter
    val get_entry_filter : unit -> log_filter option
    val set_entry_filter : log_filter -> unit
  end
  val logr : ('a, Format.formatter, unit) format -> 'a
  val log :
    label:string ->
    ?modul:string ->
    ?header_style:[< `Bold | `Italic | `Thin | `Underline ] option ->
    ?header_color:[< `Black
                 | `Blue
                 | `Bright_black
                 | `Bright_blue
                 | `Bright_cyan
                 | `Bright_green
                 | `Bright_magenta
                 | `Bright_red
                 | `Bright_yellow
                 | `Cyan
                 | `Green
                 | `Magenta
                 | `Red
                 | `White
                 | `Yellow
                   > `Magenta ] ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
  val verbose :
    ?modul:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  val info :
    ?modul:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  val warn :
    ?modul:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  val debug :
    ?modul:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  val error :
    ?modul:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  module Pervasives :
  sig
    val debug :
      ?modul:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
    val info :
      ?modul:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  end
end
val debug :
  ?modul:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
val info :
  ?modul:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
module Runtime_info :
sig
  type runtime_type =
    [ `buckle_script
    | `classic_ocaml of [ `bytecode | `native ]
    | `js_of_ocaml
    | `unknown of string ]
  val pp_runtime_type : Format.formatter -> runtime_type -> unit
  val current_runtime_type : runtime_type
end
type backtrace_info =
  [ `ocaml_backtrace of Printexc.raw_backtrace
  | `string_stacktrace of string ]
module Backtrace_info :
sig type t = backtrace_info val pp : ppf -> t -> unit end
module type Io_style =
  sig
    type 'x t
    val return : 'x -> 'x t
    val bind : 'x t -> ('x -> 'y t) -> 'y t
    val inject_error : exn -> 'x t
    val inject_error' : exn * backtrace_info option -> 'x t
    val extract_error : 'x t -> ('x, exn * backtrace_info option) result t
    val trace : string -> unit t
  end
module Direct_io :
sig
  type 'x t = ('x, exn * Backtrace_info.t) result
  val return : 'x -> 'x t
  val bind : 'x t -> ('x -> 'y t) -> 'y t
  val inject_error' : exn * backtrace_info option -> 'x t
  val inject_error : exn -> 'x t
  val extract_error : 'x t -> ('x, exn * backtrace_info option) result t
  val trace : string -> (unit, 'a) result
end
module CheckDirectIo : Io_style

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
end

module Json_JCSnafi : sig
  open Json

  val is_encodable_str : string -> bool
  val is_encodable_num : float -> bool
  val is_encodable : jv -> bool
  val unparse_jcsnafi : jv -> string
  val compare_field_name : string -> string -> int
end

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
end

module Base64 : sig
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

  module Config_rfc4648 : Config
  module Config_rfc4648_relaxed : Config
  module Config_rfc4648_url : Config
  module Config_rfc4648_url_relaxed : Config

  module Make : functor (_ : Config) -> T

  module Url : T
  include module type of Make(Config_rfc4648_relaxed)
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
end
