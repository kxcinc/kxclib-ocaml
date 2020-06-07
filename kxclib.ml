type ('a, 'b) either = Left of 'a | Right of 'b
let left x = Left x
let right x = Right x

let inc n = n + 1
let dec n = n - 1
let refset x r = r := x
let refupdate f r = r := f !r
let incr = refupdate inc
let decr = refupdate dec

let iota = function
  | 0 -> []
  | k -> 0 :: (List.init (dec k) inc)

let const c = fun _ -> c
(** constant function *)

let id x = x
(** identity function *)

let (%) f g x = x |> g |> f
(** function composition *)

let (|&>) list f = List.map f list
(** list mapping *)

let foldl = List.fold_left
(** {!List.fold_right} *)

let foldr = List.fold_right
(** {!List.fold_left} *)

let debug fmt = Format.(
    let ppf = err_formatter in
    eprintf "[DEBUG:%s] " __FILE__;
    kfprintf (fun _ -> eprintf "\n";
                       pp_print_flush ppf ();
                       flush stderr) ppf fmt)

module List = struct
  include List
  let foldl = foldl
  let foldr = foldr
  let count pred list =
    foldl (fun count x -> if pred x then inc count else count) 0 list
  (** [pred list] returns the number of elements [e] in [list] that satisfies [pred] *)

  let last list =
    foldl (fun _ x -> x) (List.hd list) list
  (** last element of list *)

  (* XXX to be optimized *)
  let fmap f l = map f l |> concat
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
end

module Hashtbl = struct
  include Hashtbl
  let rev : ('a, 'b) t -> ('b, 'a) t = fun orig ->
    to_seq orig |> Seq.map (fun (k,v) -> (v,k)) |> of_seq
  (** [rev orig] reverse the key and value of [orig] *)
end

