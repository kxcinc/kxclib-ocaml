(* adopted from OCaml 4.12.0 source tree *)
(* FIXME - sort out licensing before releasing *)

module Kxclib_comp_re = struct

module BaseCompInternals = struct
  type _jsobj
  let _flip f a b = f b a
  external _cast : 'a -> 'b = "%identity"
  let _obj_make() : _jsobj = Js.Obj.empty() |> _cast
  let _obj_set : 'a 'b. _jsobj -> 'a -> 'b -> unit =
    fun o k v -> [%raw "function(o, k, v){o[k] = v}"] o (_cast k) (_cast v)
  let _obj_get : 'a 'b . _jsobj -> 'a -> 'b =
    fun o k -> [%raw "function(o, k){return o[k]}"] o (_cast k) |> _cast
  let _stringify : 'a. 'a -> string = fun x -> [%raw "function(x){return ''+x;}"] (_cast x)
end open BaseCompInternals

module Seq = struct
type +'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

let empty () = Nil

let return x () = Cons (x, empty)

let cons x next () = Cons (x, next)

let rec append seq1 seq2 () =
  match seq1() with
  | Nil -> seq2()
  | Cons (x, next) -> Cons (x, append next seq2)

let rec map f seq () = match seq() with
  | Nil -> Nil
  | Cons (x, next) -> Cons (f x, map f next)

let rec filter_map f seq () = match seq() with
  | Nil -> Nil
  | Cons (x, next) ->
      match f x with
        | None -> filter_map f next ()
        | Some y -> Cons (y, filter_map f next)

let rec filter f seq () = match seq() with
  | Nil -> Nil
  | Cons (x, next) ->
      if f x
      then Cons (x, filter f next)
      else filter f next ()

let rec flat_map f seq () = match seq () with
  | Nil -> Nil
  | Cons (x, next) ->
    flat_map_app f (f x) next ()

(* this is [append seq (flat_map f tail)] *)
and flat_map_app f seq tail () = match seq () with
  | Nil -> flat_map f tail ()
  | Cons (x, next) ->
    Cons (x, flat_map_app f next tail)

let fold_left f acc seq =
  let rec aux f acc seq = match seq () with
    | Nil -> acc
    | Cons (x, next) ->
        let acc = f acc x in
        aux f acc next
  in
  aux f acc seq

let iter f seq =
  let rec aux seq = match seq () with
    | Nil -> ()
    | Cons (x, next) ->
        f x;
        aux next
  in
  aux seq

let rec unfold f u () =
  match f u with
  | None -> Nil
  | Some (x, u') -> Cons (x, unfold f u')
end

module Option = struct
type 'a t = 'a option = None | Some of 'a

let or_exn e = function None -> raise e | Some x -> x
let or_not_found o = or_exn Not_found o

let none = None
let some v = Some v
let value o ~default = match o with Some v -> v | None -> default
let get = function Some v -> v | None -> invalid_arg "option is None"
let bind o f = match o with None -> None | Some v -> f v
let join = function Some o -> o | None -> None
let map f o = match o with None -> None | Some v -> Some (f v)
let fold ~none ~some = function Some v -> some v | None -> none
let iter f = function Some v -> f v | None -> ()
let is_none = function None -> true | Some _ -> false
let is_some = function None -> false | Some _ -> true

let equal eq o0 o1 = match o0, o1 with
| Some v0, Some v1 -> eq v0 v1
| None, None -> true
| _ -> false

let compare cmp o0 o1 = match o0, o1 with
| Some v0, Some v1 -> cmp v0 v1
| None, None -> 0
| None, Some _ -> -1
| Some _, None -> 1

let to_result ~none = function None -> Error none | Some v -> Ok v
let to_list = function None -> [] | Some v -> [v]
let to_seq = function None -> Seq.empty | Some v -> Seq.return v
end

module Array = struct
  type 'a t = 'a array

  module B = Belt.Array

  module Imported = struct
    external _fill_1 : 'a array -> 'a -> 'a array = "fill"
    [@@bs.send]
    external _fill_3 : 'a array -> 'a -> (* start *) int -> (* end *) int -> 'a array = "fill"
    [@@bs.send]
    external _concat_2 : 'a array -> 'a array -> 'a array = "concat"
    [@@bs.send]
  end open Imported

  let length arr = B.length arr
  let get arr = B.getExn arr
  let set arr = B.setExn arr
  let make n x =
    let arr = B.makeUninitializedUnsafe n in
    _fill_1 arr x |> ignore;
    arr
  let create n x = make n x

  module Internals = struct
    let unsafe_get arr = get arr
    let unsafe_set arr = set arr
    let unsafe_fill : 'a. 'a array -> int -> int -> 'a -> unit =
      fun arr ofs len v ->
      _fill_3 arr v ofs (ofs+len) |> ignore
    let append_prim a1 a2 = _concat_2 a1 a2
    let unsafe_sub arr ofs len =
      let arr' = B.makeUninitializedUnsafe len in
      for i = ofs to pred len do
        (arr'.(i) <- arr.(i)) |> ignore
      done;
      arr'
    let unsafe_blit src src_ofs dst dst_ofs len =
      for i = 0 to pred len do
        (dst.(dst_ofs + i) <- src.(src_ofs + i)) |> ignore
      done
  end open Internals

let init l f =
  if l = 0 then [||] else
  if l < 0 then invalid_arg "Array.init"
  (* See #6575. We could also check for maximum array size, but this depends
     on whether we create a float array or a regular one... *)
  else
   let res = create l (f 0) in
   for i = 1 to pred l do
     unsafe_set res i (f i)
   done;
   res

let copy a =
  let l = length a in if l = 0 then [||] else unsafe_sub a 0 l

let append a1 a2 =
  let l1 = length a1 in
  if l1 = 0 then copy a2
  else if length a2 = 0 then unsafe_sub a1 0 l1
  else append_prim a1 a2

let sub a ofs len =
  if ofs < 0 || len < 0 || ofs > length a - len
  then invalid_arg "Array.sub"
  else unsafe_sub a ofs len

let fill a ofs len v =
  if ofs < 0 || len < 0 || ofs > length a - len
  then invalid_arg "Array.fill"
  else unsafe_fill a ofs len v

let blit a1 ofs1 a2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length a1 - len
             || ofs2 < 0 || ofs2 > length a2 - len
  then invalid_arg "Array.blit"
  else unsafe_blit a1 ofs1 a2 ofs2 len

let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

let iter2 f a b =
  if length a <> length b then
    invalid_arg "Array.iter2: arrays must have the same length"
  else
    for i = 0 to length a - 1 do f (unsafe_get a i) (unsafe_get b i) done

let map f a =
  let l = length a in
  if l = 0 then [||] else begin
    let r = create l (f(unsafe_get a 0)) in
    for i = 1 to l - 1 do
      unsafe_set r i (f(unsafe_get a i))
    done;
    r
  end

let map2 f a b =
  let la = length a in
  let lb = length b in
  if la <> lb then
    invalid_arg "Array.map2: arrays must have the same length"
  else begin
    if la = 0 then [||] else begin
      let r = create la (f (unsafe_get a 0) (unsafe_get b 0)) in
      for i = 1 to la - 1 do
        unsafe_set r i (f (unsafe_get a i) (unsafe_get b i))
      done;
      r
    end
  end

let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let mapi f a =
  let l = length a in
  if l = 0 then [||] else begin
    let r = create l (f 0 (unsafe_get a 0)) in
    for i = 1 to l - 1 do
      unsafe_set r i (f i (unsafe_get a i))
    done;
    r
  end

let to_list a =
  let rec tolist i res =
    if i < 0 then res else tolist (i - 1) (unsafe_get a i :: res) in
  tolist (length a - 1) []

(* Cannot use List.length here because the List module depends on Array. *)
let rec list_length accu = function
  | [] -> accu
  | _::t -> list_length (succ accu) t

let of_list = function
    [] -> [||]
  | hd::tl as l ->
      let a = create (list_length 0 l) hd in
      let rec fill i = function
          [] -> a
        | hd::tl -> unsafe_set a i hd; fill (i+1) tl in
      fill 1 tl

let fold_left f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let fold_right f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f (unsafe_get a i) !r
  done;
  !r

let exists p a =
  let n = length a in
  let rec loop i =
    if i = n then false
    else if p (unsafe_get a i) then true
    else loop (succ i) in
  loop 0

let for_all p a =
  let n = length a in
  let rec loop i =
    if i = n then true
    else if p (unsafe_get a i) then loop (succ i)
    else false in
  loop 0

let for_all2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Array.for_all2"
  else let rec loop i =
    if i = n1 then true
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
    else false in
  loop 0

let exists2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Array.exists2"
  else let rec loop i =
    if i = n1 then false
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then true
    else loop (succ i) in
  loop 0

let mem x a =
  let n = length a in
  let rec loop i =
    if i = n then false
    else if compare (unsafe_get a i) x = 0 then true
    else loop (succ i) in
  loop 0

let memq x a =
  let n = length a in
  let rec loop i =
    if i = n then false
    else if x == (unsafe_get a i) then true
    else loop (succ i) in
  loop 0

exception Bottom of int
let sort cmp a =
  let maxson l i =
    let i31 = i+i+i+1 in
    let x = ref i31 in
    if i31+2 < l then begin
      if cmp (get a i31) (get a (i31+1)) < 0 then x := i31+1;
      if cmp (get a !x) (get a (i31+2)) < 0 then x := i31+2;
      !x
    end else
      if i31+1 < l && cmp (get a i31) (get a (i31+1)) < 0
      then i31+1
      else if i31 < l then i31 else raise (Bottom i)
  in
  let rec trickledown l i e =
    let j = maxson l i in
    if cmp (get a j) e > 0 then begin
      set a i (get a j);
      trickledown l j e;
    end else begin
      set a i e;
    end;
  in
  let trickle l i e = try trickledown l i e with Bottom i -> set a i e in
  let rec bubbledown l i =
    let j = maxson l i in
    set a i (get a j);
    bubbledown l j
  in
  let bubble l i = try bubbledown l i with Bottom i -> i in
  let rec trickleup i e =
    let father = (i - 1) / 3 in
    assert (i <> father);
    if cmp (get a father) e < 0 then begin
      set a i (get a father);
      if father > 0 then trickleup father e else set a 0 e;
    end else begin
      set a i e;
    end;
  in
  let l = length a in
  for i = (l + 1) / 3 - 1 downto 0 do trickle l i (get a i); done;
  for i = l - 1 downto 2 do
    let e = (get a i) in
    set a i (get a 0);
    trickleup (bubble i 0) e;
  done;
  if l > 1 then (let e = (get a 1) in set a 1 (get a 0); set a 0 e)


let cutoff = 5
let stable_sort cmp a =
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if cmp s1 s2 <= 0 then begin
        set dst d s1;
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 (get a i1) i2 s2 (d + 1)
        else
          blit src2 i2 dst (d + 1) (src2r - i2)
      end else begin
        set dst d s2;
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 (get src2 i2) (d + 1)
        else
          blit a i1 dst (d + 1) (src1r - i1)
      end
    in loop src1ofs (get a src1ofs) src2ofs (get src2 src2ofs) dstofs;
  in
  let isortto srcofs dst dstofs len =
    for i = 0 to len - 1 do
      let e = (get a (srcofs + i)) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && cmp (get dst !j) e > 0) do
        set dst (!j + 1) (get dst !j);
        decr j;
      done;
      set dst (!j + 1) e;
    done;
  in
  let rec sortto srcofs dst dstofs len =
    if len <= cutoff then isortto srcofs dst dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst (dstofs + l1) l2;
      sortto srcofs a (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
    end;
  in
  let l = length a in
  if l <= cutoff then isortto 0 a 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = make l2 (get a 0) in
    sortto l1 t 0 l2;
    sortto 0 a l2 l1;
    merge l2 l1 t 0 l2 a 0;
  end


let fast_sort = stable_sort

(** {1 Iterators} *)

let to_seq a =
  let rec aux i () =
    if i < length a
    then
      let x = unsafe_get a i in
      Seq.Cons (x, aux (i+1))
    else Seq.Nil
  in
  aux 0

let to_seqi a =
  let rec aux i () =
    if i < length a
    then
      let x = unsafe_get a i in
      Seq.Cons ((i,x), aux (i+1))
    else Seq.Nil
  in
  aux 0

let of_rev_list = function
    [] -> [||]
  | hd::tl as l ->
      let len = list_length 0 l in
      let a = create len hd in
      let rec fill i = function
          [] -> a
        | hd::tl -> unsafe_set a i hd; fill (i-1) tl
      in
      fill (len-2) tl

let of_seq i =
  let l = Seq.fold_left (fun acc x -> x::acc) [] i in
  of_rev_list l
end

module Result = struct
type ('a, 'e) t = ('a, 'e) result = Ok of 'a | Error of 'e

let ok v = Ok v
let error e = Error e
let value r ~default = match r with Ok v -> v | Error _ -> default
let get_ok = function Ok v -> v | Error _ -> invalid_arg "result is Error _"
let get_error = function Error e -> e | Ok _ -> invalid_arg "result is Ok _"
let bind r f = match r with Ok v -> f v | Error _ as e -> e
let join = function Ok r -> r | Error _ as e -> e
let map f = function Ok v -> Ok (f v) | Error _ as e -> e
let map_error f = function Error e -> Error (f e) | Ok _ as v -> v
let fold ~ok ~error = function Ok v -> ok v | Error e -> error e
let iter f = function Ok v -> f v | Error _ -> ()
let iter_error f = function Error e -> f e | Ok _ -> ()
let is_ok = function Ok _ -> true | Error _ -> false
let is_error = function Error _ -> true | Ok _ -> false

let equal ~ok ~error r0 r1 = match r0, r1 with
| Ok v0, Ok v1 -> ok v0 v1
| Error e0, Error e1 -> error e0 e1
| _, _ -> false

let compare ~ok ~error r0 r1 = match r0, r1 with
| Ok v0, Ok v1 -> ok v0 v1
| Error e0, Error e1 -> error e0 e1
| Ok _, Error _ -> -1
| Error _, Ok _ -> 1

let to_option = function Ok v -> Some v | Error _ -> None
let to_list = function Ok v -> [v] | Error _ -> []
let to_seq = function Ok v -> Seq.return v | Error _ -> Seq.empty
end
module List = struct
module B = Belt.List
type 'a t = 'a list = [] | (::) of 'a * 'a list
let length l = B.length l
let hd l = B.head l |> Option.or_exn (Failure "hd")
let tl l = B.tail l |> Option.or_exn (Failure "tl")
let nth_opt l n = B.get l n
let nth l n = nth_opt l n |> Option.or_exn (Invalid_argument "List.nth")
let append = (@)
let rev_append l1 l2 = B.reverseConcat l1 l2
let rev l = B.reverse l
let init len f = B.makeBy len f
let flatten ls = B.flatten ls
let concat = flatten
let map f l = B.map l f
let mapi f l = B.mapWithIndex l f
let rev_map f l = B.mapReverse l f
let iter f l = B.forEach l f
let iteri f l = B.forEachWithIndex l f
let fold_left f accu l = B.reduce l accu f
let fold_right f l accu = B.reduceReverse l accu (_flip f)
let map2 f l1 l2 = B.zipBy l1 l2 f
let rev_map2 f l1 l2 = B.mapReverse2 l1 l2 f
let iter2 f l1 l2 = B.forEach2 l1 l2 f
let for_all p l = B.every l p
let exists p l = B.some l p
let for_all2 p l1 l2 = B.every2 l1 l2 p
let exists2 p l1 l2 = B.some2 l1 l2 p
let mem x l = exists ((=) x) l
let memq x l = exists ((==) x) l
let assoc_opt k l = B.getAssoc l k (=)
let assq_opt k l = B.getAssoc l k (==)
let assoc k l = assoc_opt k l |> Option.or_not_found
let assq k l = assq_opt k l |> Option.or_not_found
let mem_assoc x l = B.hasAssoc l x (=)
let mem_assq x l = B.hasAssoc l x (==)
let remove_assoc x l = B.removeAssoc l x (=)
let remove_assq x l = B.removeAssoc l x (==)
let find_opt p l = B.getBy l p
let find p l = find_opt p l |> Option.or_not_found
let find_map f l =
  let (>>=) = Option.bind in
  find_opt (fun x -> match f x with Some _ -> true | None -> false) l
  >>= f
let find_all p l = B.keep l p
let filter = find_all
let filteri p l = B.keepWithIndex l (_flip p)
let filter_map f l = B.keepMap l f
let concat_map f l =
  let rec aux f acc = function
    | [] -> rev acc
    | x :: l ->
       let xs = f x in
       aux f (rev_append xs acc) l
  in aux f [] l

let fold_left_map f accu l =
  let rec aux accu l_accu = function
    | [] -> accu, rev l_accu
    | x :: l ->
        let accu, x = f accu x in
        aux accu (x :: l_accu) l in
  aux accu [] l

let partition p l = B.partition l p
let split l = B.unzip l
let combine l1 l2 = B.zip l1 l2
let rec merge cmp l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      if cmp h1 h2 <= 0
      then h1 :: merge cmp t1 l2
      else h2 :: merge cmp l1 t2
let stable_sort cmp l = B.sort l cmp
let sort = stable_sort
let fast_sort = stable_sort
let compare cmp l1 l2 = B.cmp l1 l2 cmp
let equal eq l1 l2 = B.eq l1 l2 eq

let to_seq l =
  let rec aux l () = match l with
    | [] -> Seq.Nil
    | x :: tail -> Seq.Cons (x, aux tail)
  in
  aux l

let of_seq seq =
  let rec direct depth seq : _ list =
    if depth=0
    then
      Seq.fold_left (fun acc x -> x::acc) [] seq
      |> rev (* tailrec *)
    else match seq() with
      | Seq.Nil -> []
      | Seq.Cons (x, next) -> x :: direct (depth-1) next
  in
  direct 500 seq
end
module Hashtbl = struct
  include Hashtbl
  let add_seq tbl i =
    Seq.iter (fun (k,v) -> add tbl k v) i

  let replace_seq tbl i =
    Seq.iter (fun (k,v) -> replace tbl k v) i

  let of_seq i =
    let tbl = create 16 in
    replace_seq tbl i;
    tbl

let to_seq tbl =
  (* kinda inefficient, but whatever *)
  fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.to_seq

let to_seq_keys m = Seq.map fst (to_seq m)

let to_seq_values m = Seq.map snd (to_seq m)

end

module Bytes = struct
  include Bytes

  let get_uint8 buf i =
    get buf i |> int_of_char

let to_seq s =
  let rec aux i () =
    if i = length s then Seq.Nil
    else
      let x = get s i in
      Seq.Cons (x, aux (i+1))
  in
  aux 0

let to_seqi s =
  let rec aux i () =
    if i = length s then Seq.Nil
    else
      let x = get s i in
      Seq.Cons ((i,x), aux (i+1))
  in
  aux 0

let of_seq i =
  let n = ref 0 in
  let buf = ref (make 256 '\000') in
  let resize () =
    (* resize *)
    let new_len = min (2 * length !buf) Sys.max_string_length in
    if length !buf = new_len then failwith "Bytes.of_seq: cannot grow bytes";
    let new_buf = make new_len '\000' in
    blit !buf 0 new_buf 0 !n;
    buf := new_buf
  in
  Seq.iter
    (fun c ->
       if !n = length !buf then resize();
       set !buf !n c;
       incr n)
    i;
  sub !buf 0 !n

end

module String = struct
include String
module B = Bytes
let bos = B.unsafe_of_string
let bts = B.unsafe_to_string
let to_seq s = bos s |> B.to_seq
let to_seqi s = bos s |> B.to_seqi
let of_seq g = B.of_seq g |> bts
end

module Int = struct
type t = int
let zero = 0
let one = 1
let minus_one = -1
let neg x = (-x)
let add = (+)
let sub = (-)
let mul = ( * )
let div = ( / )
let rem = (mod)
let succ = succ
let pred = pred
let abs x = if x >= 0 then x else -x

module Imported = struct
  external _MAX_SAFE_INTEGER : int = "MAX_SAFE_INTEGER"
  [@@bs.val][@@bs.scope "Number"]
  external _MIN_SAFE_INTEGER : int = "MIN_SAFE_INTEGER"
  [@@bs.val][@@bs.scope "Number"]
end open Imported

let max_int = _MAX_SAFE_INTEGER
let min_int = _MIN_SAFE_INTEGER
end

module Float = struct
  module Imported = struct
    external _isInteger : float -> bool = "isInteger"
    [@@bs.val][@@bs.scope "Number"]
  end open Imported

  let ceil = Js.Math.ceil_float
  let floor = Js.Math.floor_float
  let is_integer = _isInteger
  let to_int : float -> int = fun x -> _cast x
end

module Buffer = struct
type t =
 {mutable buffer : bytes;
  mutable position : int;
  mutable length : int;
  initial_buffer : bytes}
(* Invariants: all parts of the code preserve the invariants that:
   - [0 <= b.position <= b.length]
   - [b.length = Bytes.length b.buffer]

   Note in particular that [b.position = b.length] is legal,
   it means that the buffer is full and will have to be extended
   before any further addition. *)

let create n =
 let n = if n < 1 then 1 else n in
 let n = if n > Sys.max_string_length then Sys.max_string_length else n in
 let s = Bytes.create n in
 {buffer = s; position = 0; length = n; initial_buffer = s}

let contents b = Bytes.sub_string b.buffer 0 b.position
let to_bytes b = Bytes.sub b.buffer 0 b.position

let sub b ofs len =
  if ofs < 0 || len < 0 || ofs > b.position - len
  then invalid_arg "Buffer.sub"
  else Bytes.sub_string b.buffer ofs len


let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || srcoff > src.position - len
             || dstoff < 0 || dstoff > (Bytes.length dst) - len
  then invalid_arg "Buffer.blit"
  else
    Bytes.unsafe_blit src.buffer srcoff dst dstoff len


let nth b ofs =
  if ofs < 0 || ofs >= b.position then
   invalid_arg "Buffer.nth"
  else Bytes.unsafe_get b.buffer ofs


let length b = b.position

let clear b = b.position <- 0

let reset b =
  b.position <- 0;
  b.buffer <- b.initial_buffer;
  b.length <- Bytes.length b.buffer

(* [resize b more] ensures that [b.position + more <= b.length] holds
   by dynamically extending [b.buffer] if necessary -- and thus
   increasing [b.length].

   In particular, after [resize b more] is called, a direct access of
   size [more] at [b.position] will always be in-bounds, so that
   (unsafe_{get,set}) may be used for performance.
*)
let resize b more =
  let old_pos = b.position in
  let old_len = b.length in
  let new_len = ref old_len in
  while old_pos + more > !new_len do new_len := 2 * !new_len done;
  if !new_len > Sys.max_string_length then begin
    if old_pos + more <= Sys.max_string_length
    then new_len := Sys.max_string_length
    else failwith "Buffer.add: cannot grow buffer"
  end;
  let new_buffer = Bytes.create !new_len in
  (* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. *)
  Bytes.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer;
  b.length <- !new_len;
  assert (b.position + more <= b.length);
  assert (old_pos + more <= b.length);
  ()
  (* Note: there are various situations (preemptive threads, signals and
     gc finalizers) where OCaml code may be run asynchronously; in
     particular, there may be a race with another user of [b], changing
     its mutable fields in the middle of the [resize] call. The Buffer
     module does not provide any correctness guarantee if that happens,
     but we must still ensure that the datastructure invariants hold for
     memory-safety -- as we plan to use [unsafe_{get,set}].

     There are two potential allocation points in this function,
     [ref] and [Bytes.create], but all reads and writes to the fields
     of [b] happen before both of them or after both of them.

     We therefore assume that [b.position] may change at these allocations,
     and check that the [b.position + more <= b.length] postcondition
     holds for both values of [b.position], before or after the function
     is called. More precisely, the following invariants must hold if the
     function returns correctly, in addition to the usual buffer invariants:
     - [old(b.position) + more <= new(b.length)]
     - [new(b.position) + more <= new(b.length)]
     - [old(b.length) <= new(b.length)]

     Note: [b.position + more <= old(b.length)] does *not*
     hold in general, as it is precisely the case where you need
     to call [resize] to increase [b.length].

     Note: [assert] above does not mean that we know the conditions
     always hold, but that the function may return correctly
     only if they hold.

     Note: the other functions in this module does not need
     to be checked with this level of scrutiny, given that they
     read/write the buffer immediately after checking that
     [b.position + more <= b.length] hold or calling [resize].
  *)

let add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1

 let add_utf_8_uchar b u = match Uchar.to_int u with
 | u when u < 0 -> assert false
 | u when u <= 0x007F ->
     add_char b (Char.unsafe_chr u)
 | u when u <= 0x07FF ->
     let pos = b.position in
     if pos + 2 > b.length then resize b 2;
     Bytes.unsafe_set b.buffer (pos    )
       (Char.unsafe_chr (0xC0 lor (u lsr 6)));
     Bytes.unsafe_set b.buffer (pos + 1)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     b.position <- pos + 2
 | u when u <= 0xFFFF ->
     let pos = b.position in
     if pos + 3 > b.length then resize b 3;
     Bytes.unsafe_set b.buffer (pos    )
       (Char.unsafe_chr (0xE0 lor (u lsr 12)));
     Bytes.unsafe_set b.buffer (pos + 1)
       (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
     Bytes.unsafe_set b.buffer (pos + 2)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     b.position <- pos + 3
 | u when u <= 0x10FFFF ->
     let pos = b.position in
     if pos + 4 > b.length then resize b 4;
     Bytes.unsafe_set b.buffer (pos    )
       (Char.unsafe_chr (0xF0 lor (u lsr 18)));
     Bytes.unsafe_set b.buffer (pos + 1)
       (Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F)));
     Bytes.unsafe_set b.buffer (pos + 2)
       (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
     Bytes.unsafe_set b.buffer (pos + 3)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     b.position <- pos + 4
 | _ -> assert false

 let add_utf_16be_uchar b u = match Uchar.to_int u with
 | u when u < 0 -> assert false
 | u when u <= 0xFFFF ->
     let pos = b.position in
     if pos + 2 > b.length then resize b 2;
     Bytes.unsafe_set b.buffer (pos    ) (Char.unsafe_chr (u lsr 8));
     Bytes.unsafe_set b.buffer (pos + 1) (Char.unsafe_chr (u land 0xFF));
     b.position <- pos + 2
 | u when u <= 0x10FFFF ->
     let u' = u - 0x10000 in
     let hi = 0xD800 lor (u' lsr 10) in
     let lo = 0xDC00 lor (u' land 0x3FF) in
     let pos = b.position in
     if pos + 4 > b.length then resize b 4;
     Bytes.unsafe_set b.buffer (pos    ) (Char.unsafe_chr (hi lsr 8));
     Bytes.unsafe_set b.buffer (pos + 1) (Char.unsafe_chr (hi land 0xFF));
     Bytes.unsafe_set b.buffer (pos + 2) (Char.unsafe_chr (lo lsr 8));
     Bytes.unsafe_set b.buffer (pos + 3) (Char.unsafe_chr (lo land 0xFF));
     b.position <- pos + 4
 | _ -> assert false

 let add_utf_16le_uchar b u = match Uchar.to_int u with
 | u when u < 0 -> assert false
 | u when u <= 0xFFFF ->
     let pos = b.position in
     if pos + 2 > b.length then resize b 2;
     Bytes.unsafe_set b.buffer (pos    ) (Char.unsafe_chr (u land 0xFF));
     Bytes.unsafe_set b.buffer (pos + 1) (Char.unsafe_chr (u lsr 8));
     b.position <- pos + 2
 | u when u <= 0x10FFFF ->
     let u' = u - 0x10000 in
     let hi = 0xD800 lor (u' lsr 10) in
     let lo = 0xDC00 lor (u' land 0x3FF) in
     let pos = b.position in
     if pos + 4 > b.length then resize b 4;
     Bytes.unsafe_set b.buffer (pos    ) (Char.unsafe_chr (hi land 0xFF));
     Bytes.unsafe_set b.buffer (pos + 1) (Char.unsafe_chr (hi lsr 8));
     Bytes.unsafe_set b.buffer (pos + 2) (Char.unsafe_chr (lo land 0xFF));
     Bytes.unsafe_set b.buffer (pos + 3) (Char.unsafe_chr (lo lsr 8));
     b.position <- pos + 4
 | _ -> assert false

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset > String.length s - len
  then invalid_arg "Buffer.add_substring/add_subbytes";
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Bytes.unsafe_blit (Bytes.of_string s) offset b.buffer b.position len;
  b.position <- new_position

let add_subbytes b s offset len =
  add_substring b (Bytes.unsafe_to_string s) offset len

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Bytes.unsafe_blit (Bytes.of_string s) 0 b.buffer b.position len;
  b.position <- new_position

let add_bytes b s = add_string b (Bytes.unsafe_to_string s)

let add_buffer b bs =
  add_subbytes b bs.buffer 0 bs.position

(* this (private) function could move into the standard library *)
let really_input_up_to ic buf ofs len =
  let rec loop ic buf ~already_read ~ofs ~to_read =
    if to_read = 0 then already_read
    else begin
      let r = input ic buf ofs to_read in
      if r = 0 then already_read
      else begin
        let already_read = already_read + r in
        let ofs = ofs + r in
        let to_read = to_read - r in
        loop ic buf ~already_read ~ofs ~to_read
      end
    end
  in loop ic buf ~already_read:0 ~ofs ~to_read:len


let unsafe_add_channel_up_to b ic len =
  if b.position + len > b.length then resize b len;
  let n = really_input_up_to ic b.buffer b.position len in
  (* The assertion below may fail in weird scenario where
     threaded/finalizer code, run asynchronously during the
     [really_input_up_to] call, races on the buffer; we don't ensure
     correctness in this case, but need to preserve the invariants for
     memory-safety (see discussion of [resize]). *)
  assert (b.position + n <= b.length);
  b.position <- b.position + n;
  n

let add_channel b ic len =
  if len < 0 || len > Sys.max_string_length then   (* PR#5004 *)
    invalid_arg "Buffer.add_channel";
  let n = unsafe_add_channel_up_to b ic len in
  (* It is intentional that a consumer catching End_of_file
     will see the data written (see #6719, #7136). *)
  if n < len then raise End_of_file;
  ()

let output_buffer oc b =
  output oc b.buffer 0 b.position

let closing = function
  | '(' -> ')'
  | '{' -> '}'
  | _ -> assert false

(* opening and closing: open and close characters, typically ( and )
   k: balance of opening and closing chars
   s: the string where we are searching
   start: the index where we start the search. *)
let advance_to_closing opening closing k s start =
  let rec advance k i lim =
    if i >= lim then raise Not_found else
    if s.[i] = opening then advance (k + 1) (i + 1) lim else
    if s.[i] = closing then
      if k = 0 then i else advance (k - 1) (i + 1) lim
    else advance k (i + 1) lim in
  advance k start (String.length s)

let advance_to_non_alpha s start =
  let rec advance i lim =
    if i >= lim then lim else
    match s.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> advance (i + 1) lim
    | _ -> i in
  advance start (String.length s)

(* We are just at the beginning of an ident in s, starting at start. *)
let find_ident s start lim =
  if start >= lim then raise Not_found else
  match s.[start] with
  (* Parenthesized ident ? *)
  | '(' | '{' as c ->
     let new_start = start + 1 in
     let stop = advance_to_closing c (closing c) 0 s new_start in
     String.sub s new_start (stop - start - 1), stop + 1
  (* Regular ident *)
  | _ ->
     let stop = advance_to_non_alpha s (start + 1) in
     String.sub s start (stop - start), stop

(* Substitute $ident, $(ident), or ${ident} in s,
    according to the function mapping f. *)
let add_substitute b f s =
  let lim = String.length s in
  let rec subst previous i =
    if i < lim then begin
      match s.[i] with
      | '$' as current when previous = '\\' ->
         add_char b current;
         subst ' ' (i + 1)
      | '$' ->
         let j = i + 1 in
         let ident, next_i = find_ident s j lim in
         add_string b (f ident);
         subst ' ' next_i
      | current when previous == '\\' ->
         add_char b '\\';
         add_char b current;
         subst ' ' (i + 1)
      | '\\' as current ->
         subst current (i + 1)
      | current ->
         add_char b current;
         subst current (i + 1)
    end else
    if previous = '\\' then add_char b previous in
  subst ' ' 0

let truncate b len =
    if len < 0 || len > length b then
      invalid_arg "Buffer.truncate"
    else
      b.position <- len

(** {1 Iterators} *)

let to_seq b =
  let rec aux i () =
    (* Note that b.position is not a constant and cannot be lifted out of aux *)
    if i >= b.position then Seq.Nil
    else
      let x = Bytes.unsafe_get b.buffer i in
      Seq.Cons (x, aux (i+1))
  in
  aux 0

let to_seqi b =
  let rec aux i () =
    (* Note that b.position is not a constant and cannot be lifted out of aux *)
    if i >= b.position then Seq.Nil
    else
      let x = Bytes.unsafe_get b.buffer i in
      Seq.Cons ((i,x), aux (i+1))
  in
  aux 0

let add_seq b seq = Seq.iter (add_char b) seq

let of_seq i =
  let b = create 32 in
  add_seq b i;
  b

(** {6 Binary encoding of integers} *)

external unsafe_set_int8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"
(* external unsafe_set_int16 : bytes -> int -> int -> unit = "%caml_bytes_set16u" *)
(* external unsafe_set_int32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u" *)
(* external unsafe_set_int64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u" *)
(* external swap16 : int -> int = "%bswap16" *)
(* external swap32 : int32 -> int32 = "%bswap_int32" *)
(* external swap64 : int64 -> int64 = "%bswap_int64" *)


let add_int8 b x =
  let new_position = b.position + 1 in
  if new_position > b.length then resize b 1;
  unsafe_set_int8 b.buffer b.position x;
  b.position <- new_position

(* let add_int16_ne b x = *)
(*   let new_position = b.position + 2 in *)
(*   if new_position > b.length then resize b 2; *)
(*   unsafe_set_int16 b.buffer b.position x; *)
(*   b.position <- new_position *)

(* let add_int32_ne b x = *)
(*   let new_position = b.position + 4 in *)
(*   if new_position > b.length then resize b 4; *)
(*   unsafe_set_int32 b.buffer b.position x; *)
(*   b.position <- new_position *)

(* let add_int64_ne b x = *)
(*   let new_position = b.position + 8 in *)
(*   if new_position > b.length then resize b 8; *)
(*   unsafe_set_int64 b.buffer b.position x; *)
(*   b.position <- new_position *)

(* let add_int16_le b x = *)
(*   add_int16_ne b (if Sys.big_endian then swap16 x else x) *)

(* let add_int16_be b x = *)
(*   add_int16_ne b (if Sys.big_endian then x else swap16 x) *)

(* let add_int32_le b x = *)
(*   add_int32_ne b (if Sys.big_endian then swap32 x else x) *)

(* let add_int32_be b x = *)
(*   add_int32_ne b (if Sys.big_endian then x else swap32 x) *)

(* let add_int64_le b x = *)
(*   add_int64_ne b (if Sys.big_endian then swap64 x else x) *)

(* let add_int64_be b x = *)
(*   add_int64_ne b (if Sys.big_endian then x else swap64 x) *)

let add_uint8 = add_int8
(* let add_uint16_ne = add_int16_ne *)
(* let add_uint16_le = add_int16_le *)
(* let add_uint16_be = add_int16_be *)
end

module Json_ext = struct
  type jv = [
    | `null
    | `bool of bool
    | `num of float
    | `str of string
    | `arr of jv list
    | `obj of (string*jv) list
    ]

  type xjv
  (** external json value, simply a JavaScript value in BuckleScript runtime *)

  let rec to_xjv : jv -> xjv = function
    | `null -> Js.null |> _cast
    | `bool b -> b |> _cast
    | `num x -> x |> _cast
    | `str s -> s |> _cast
    | `arr xs -> xs |> Array.of_list |> Array.map to_xjv |> _cast
    | `obj fs ->
       let o = _obj_make() in
       List.iter (fun (k, v) ->
         _obj_set o k (to_xjv v)) fs;
       o |> _cast

  let rec of_xjv : xjv -> jv = fun x ->
    match Js.Types.classify (_cast x) with
    | JSFalse -> `bool false
    | JSTrue -> `bool true
    | JSNull -> `null
    | JSUndefined -> invalid_arg "of_xjv: 'undefined' not expected"
    | JSNumber x -> `num x
    | JSString s -> `str s
    | JSFunction f -> invalid_arg ("of_xjv: function not expected: "^(_stringify f))
    | JSObject obj ->
       if Js.Array.isArray obj then (
         let xs = (_cast obj) |> Array.map of_xjv |> Array.to_list in
         `arr xs
       ) else (
         let o = _cast obj in
         let keys = Js.Obj.keys (_cast obj) in
         let fs =
           Array.map (fun k -> k, of_xjv (_obj_get o k)) keys
           |> Array.to_list in
         `obj fs
       )
    | JSSymbol symb -> invalid_arg ("of_xjv: function not expected: "^(_stringify symb))

  let json_of_xjv : xjv -> string = fun xjv ->
    Js.Json.stringify (xjv |> _cast)

  let json_of_jv : jv -> string = fun jv ->
    jv |> to_xjv |> json_of_xjv

  let xjv_of_json_exn : string -> xjv = fun json ->
    Js.Json.parseExn json |> _cast

  let xjv_of_json : string -> xjv option = fun json ->
    try xjv_of_json_exn json |> Option.some
    with _ -> None

  let jv_of_json_exn : string -> jv = fun json ->
    json |> xjv_of_json_exn |> of_xjv

  let jv_of_json : string -> jv option = fun json ->
    json |> xjv_of_json |> Option.map of_xjv

  module TestSamples = struct
    (* TODO - move to another file *)

    let obj00 : jv = (`obj [] : jv)
    let obj01 : jv =
      (`obj [
           "name", `str "James Smith";
           "age", `num 12.;
         ] : jv)
    let obj02 : jv =
      (`obj [
           "name", `str "James Smith";
           "age", `num 12.;
           "spouse", `null;
           "parents", `arr [`str "Amy Smith"; `str "Bob Smith"];
         ] : jv)

    let xjv_conv_obj00 = obj00 |> to_xjv
    let xjv_conv_obj01 = obj01 |> to_xjv
    let xjv_conv_obj02 = obj02 |> to_xjv
  end
end

module Printexc = struct
  include[@warning "-3"] Printexc
  type raw_backtrace = string list
  let raw_backtrace_to_string : raw_backtrace -> string = fun bt ->
    String.concat "\n" bt
  let get_raw_backtrace() : raw_backtrace =
    let stacktrace: string = [%raw "(()=>{try { throw new Error();}catch(e){return e.stack}})()"] in
    stacktrace |> String.split_on_char '\n'
end

module JsInterop = struct
  let bytes0 = Bytes.init 12 (fun i -> char_of_int i)
  let bytes_of_uint8array : Js.TypedArray2.Uint8Array.t -> Bytes.t = fun arr ->
    let module Arr = Js.TypedArray2.Uint8Array in
    let len = Arr.byteLength arr in
    Bytes.init len (fun i -> Arr.unsafe_get arr i |> char_of_int)
  let uint8array_of_bytes : Bytes.t -> Js.TypedArray2.Uint8Array.t = fun bytes ->
    let module Buf = Js.TypedArray2.ArrayBuffer in
    let module Arr = Js.TypedArray2.Uint8Array in
    let len = Bytes.length bytes in
    let buf = Buf.make len in
    let arr = Arr.fromBuffer buf in
    for i = 0 to pred len do
      Arr.unsafe_set arr i (Bytes.get_uint8 bytes i)
    done;
    arr
end

end (* Kxclib_comp_re *)

module Kxclib_comp = struct
  (* perhaps due to a bug of ReScript compiler, simply using a module alias would produce
     an undefined variable Kxclib_comp in the compiled js file *)
  include Kxclib_comp_re
end


open Kxclib_comp

[@@@warning "-3-44"]
[@@@ocaml.ppx.context
  {
    tool_name = "ppx_driver";
    include_dirs = [];
    load_path = [];
    open_modules = [];
    for_package = None;
    debug = false;
    use_threads = false;
    use_vmthreads = false;
    recursive_types = false;
    principal = false;
    transparent_modules = false;
    unboxed_types = false;
    unsafe_string = false;
    cookies =
      [("library-name", "kxclib_src");
      ("ppx_optcomp.env",
        (env ~flambda2:(Defined false) ~flambda_backend:(Defined false)
           ~ocaml_version:(Defined (4, 12, 1)) ~os_type:(Defined "re")
           ~re:(Defined true)))]
  }]
let refset r x = r := x[@@ocaml.doc " [refset r x] sets [x] to ref [r]. "]
let refupdate r f = r := (f (!r))[@@ocaml.doc
                                   " [refupdate r f] updates referent of [r] by [f]. "]
let refappend r x = r := (x :: (!r))[@@ocaml.doc
                                      " [refappend r x] appends [x] to referent of [r]. "]
let refupdate' f r = r := (f (!r))[@@ocaml.doc
                                    " [refupdate' f r] is equivalent to [refupdate r f]. "]
let refappend' x r = r := (x :: (!r))[@@ocaml.doc
                                       " [refappend' x r] is equivalent to [refappend r x]. "]
let refpop r = match !r with | h::t -> (r := t; h) | [] -> raise Not_found
  [@@ocaml.doc
    " [refpop r] pop first item of the list referred to by [r].\n    {b Raises} [Not_found] if the list is empty. "]
let incr = refupdate' succ[@@ocaml.doc
                            " [incr r] increases the referent of [r] by one. "]
let decr = refupdate' pred[@@ocaml.doc
                            " [decr r] decreases the referent of [r] by one. "]
let refupdate'_and_get f r = r := (f (!r)); !r
let get_and_refupdate' f r = let x = !r in r := (f (!r)); x
let incr_and_get = refupdate'_and_get succ
let decr_and_get = refupdate'_and_get pred
let get_and_incr = get_and_refupdate' succ
let get_and_decr = get_and_refupdate' pred
let constant c _ = c[@@ocaml.doc " constant function "]
let identity x = x[@@ocaml.doc " identity function "]
let failwith' fmt = Format.kasprintf failwith fmt
let invalid_arg' fmt = Format.kasprintf invalid_arg fmt
let iotaf n func =
  let rec loop acc =
    function | m when m = n -> acc | m -> loop ((func m) :: acc) (succ m) in
  (loop [] 0) |> List.rev
let iotaf' n func =
  let rec loop = function | m when m = n -> () | m -> (func m; loop (succ m)) in
  loop 0
let iotafl n func acc0 =
  let rec loop acc =
    function | m when m = n -> acc | m -> loop (func acc m) (succ m) in
  loop acc0 0
let min_by f x y = if (f y) > (f x) then x else y
let max_by f x y = if (f y) < (f x) then x else y
module Functionals =
  struct
    let negate pred x = not (pred x)[@@ocaml.doc " negate a predicate "]
    let both p g x = (p x) && (g x)
    let either p g x = (p x) || (g x)
    let dig2nd f a b = f b a[@@ocaml.doc
                              " [f] dig the second argument of [f] to be the first. aka [flip] "]
    let dig3rd f a b c = f c a b[@@ocaml.doc
                                  " [f] dig the third argument of [f] to be the first "]
    let flip = dig2nd[@@ocaml.doc
                       " [f] flip the first arguments of [f]. aka [dig2nd] "]
    let fix1st x f = f x[@@ocaml.doc
                          " [x f] fix the first argument to [f] as [x] "]
    let fix2nd y f x = f x y[@@ocaml.doc
                              " [y f] fix the second argument to [f] as [y] "]
    let fix3rd z f x y = f x y z[@@ocaml.doc
                                  " [z f] fix the third argument to [f] as [z] "]
    let fix1st' x f _ = f x[@@ocaml.doc
                             " [x f] fix the first argument to [f] as [x], but still accept (and ignore) the fixed argument "]
    let tap f x = f x; x
    let reptill judge f x =
      let rec loop y = if judge y then y else loop (f x) in loop (f x)
      [@@ocaml.doc
        " [reptill judge f x] evaluates [f x] repeatedly till [judge (f x)] holds. "]
    let ntimes n f x =
      let rec loop acc = function | 0 -> acc | n -> loop (f acc) (n - 1) in
      loop x n[@@ocaml.doc " [ntimes n f x] applies [f] ntimes to [x]. "]
    let dotill judge f x =
      let rec loop y = if judge y then y else loop (f y) in loop (f x)
      [@@ocaml.doc
        " [dotill judge f x] applies [f] to [x] repeatedly till [judge x] holds. "]
    let fixpoint ?maxn  =
      match maxn with
      | None ->
          (fun f ->
             fun x ->
               let rec loop (x, x') = if x = x' then x else loop (x', (f x')) in
               loop (x, (f x)))
      | Some 0 -> (fun _ -> fun x -> x)
      | Some n ->
          (fun f ->
             fun x ->
               let rec loop n (x, x') =
                 if n = 0
                 then x'
                 else if x = x' then x else loop (pred n) (x', (f x')) in
               loop (pred n) (x, (f x)))[@@ocaml.doc
                                          " [fixpoint f] try to resolve the fixpoint of f.\n      [maxn], an optional argument, limits the number of iterations\n      to find the fix point. "]
    let converge' judge f =
      let rec loop n (x, x') =
        match judge n x x' with
        | true -> Ok x'
        | false -> loop (succ n) (x', (f x')) in
      fun x -> loop 1 (x, (f x))
    let converge judge f x =
      converge' (fun _ -> fun x -> fun x' -> judge x x') f x
    module BasicInfix =
      struct
        let (%) : ('y -> 'z) -> ('x -> 'y) -> 'x -> 'z =
          fun f -> fun g -> fun x -> (x |> g) |> f[@@ocaml.doc
                                                    " function composition 1 "]
        let (%%) : ('a -> 'y -> 'z) -> ('x -> 'y) -> 'a -> 'x -> 'z =
          fun f -> fun g -> fun x -> fun y -> f x (g y)[@@ocaml.doc
                                                         " function composition 1, on second argument "]
        let (&>) : ('x -> 'y) -> ('y -> 'z) -> 'x -> 'z =
          fun g -> fun f -> fun x -> (x |> g) |> f[@@ocaml.doc
                                                    " function composition 2 "]
        let (&&>) : ('x -> 'y -> 'z) -> ('z -> 'r) -> 'x -> 'y -> 'r =
          fun g -> fun f -> fun x -> fun y -> (g x y) |> f[@@ocaml.doc
                                                            " function composition 2, arity=2 "]
        let (|->) : 'x -> ('x -> unit) -> 'x = fun x -> fun f -> f x; x
          [@@ocaml.doc " piping with tapping "]
        let (//) : ('a -> 'x) -> ('b -> 'y) -> ('a * 'b) -> ('x * 'y) =
          fun fa -> fun fb -> fun (a, b) -> ((fa a), (fb b))
        let (/>) : ('a * 'b) -> ('b -> 'c) -> ('a * 'c) =
          fun (a, b) -> fun f -> (a, (f b))
        let (/<) : ('a * 'b) -> ('a -> 'c) -> ('c * 'b) =
          fun (a, b) -> fun f -> ((f a), b)
        let (?>) : ('b -> 'c) -> ('a * 'b) -> ('a * 'c) =
          fun f -> fun (a, b) -> (a, (f b))[@@ocaml.doc " lift to snd "]
        let (?<) : ('a -> 'c) -> ('a * 'b) -> ('c * 'b) =
          fun f -> fun (a, b) -> ((f a), b)[@@ocaml.doc " lift to fst "]
        let (!!) : ('a -> 'b -> 'x) -> ('a * 'b) -> 'x =
          fun f -> fun (a, b) -> f a b[@@ocaml.doc " uncurry "]
        let (!?) : (('a * 'b) -> 'x) -> 'a -> 'b -> 'x =
          fun f -> fun a -> fun b -> f (a, b)[@@ocaml.doc " curry "]
      end
    module CommonTypes = struct type 'x endo = 'x -> 'x end
    module Infix = BasicInfix
  end
module Fn = Functionals
include Functionals.BasicInfix
include Functionals.CommonTypes
module PipeOps(S:sig
                   type _ t
                   val map : ('x -> 'y) -> 'x t -> 'y t
                   val iter : ('x -> unit) -> 'x t -> unit
                   val fold_left :
                     ('acc -> 'x -> 'acc) -> 'acc -> 'x t -> 'acc
                   val filter : ('x -> bool) -> 'x t -> 'x t
                   val filter_map : ('x -> 'y option) -> 'x t -> 'y t
                 end) =
  struct
    open S
    let (|&>) : 'x t -> ('x -> 'y) -> 'y t = fun xs -> fun f -> map f xs
      [@@ocaml.doc " piping map "]
    let (|+&>) : 'x t -> ('x -> 'y) -> ('x * 'y) t =
      fun xs -> fun f -> map (fun x -> (x, (f x))) xs[@@ocaml.doc
                                                       " piping map to snd "]
    let (|!>) : 'x t -> ('x -> unit) -> unit = fun xs -> fun f -> iter f xs
      [@@ocaml.doc " piping iter "]
    let (|-!>) : 'x t -> ('x -> unit) -> 'x t =
      fun xs -> fun f -> iter f xs; xs[@@ocaml.doc
                                        " piping and iter-tapping "]
    let (|@>) : 'x t -> ('acc * (('acc * 'x) -> 'acc)) -> 'acc =
      fun xs -> fun (z, f) -> fold_left (fun acc -> fun x -> f (acc, x)) z xs
      [@@ocaml.doc " piping fold_left "]
    let (|?>) : 'x t -> ('x -> bool) -> 'x t = fun xs -> fun f -> filter f xs
      [@@ocaml.doc " piping filter "]
    let (|&?>) : 'x t -> ('x -> 'y option) -> 'y t =
      fun xs -> fun f -> filter_map f xs[@@ocaml.doc " piping filter map "]
    let (|+&?>) : 'x t -> ('x -> 'y option) -> ('x * 'y) t =
      fun xs ->
        fun f ->
          filter_map
            (fun x -> match f x with | Some y -> Some (x, y) | None -> None)
            xs[@@ocaml.doc " piping filter map to snd "]
  end
module type Monadic  =
  sig
    type _ t
    val return : 'x -> 'x t
    val bind : 'x t -> ('x -> 'y t) -> 'y t
  end
module MonadOps(M:sig
                    type _ t
                    val return : 'x -> 'x t
                    val bind : 'x t -> ('x -> 'y t) -> 'y t
                  end) =
  struct
    let return x = M.return x
    let (>>=) = M.bind
    let (>>) : 'x M.t -> 'y M.t -> 'y M.t =
      fun ma -> fun mb -> ma >>= (fun _ -> mb)
    let (>|=) : 'x M.t -> ('x -> 'y) -> 'y M.t =
      fun ma -> fun f -> ma >>= (fun x -> return (f x))
    let sequence_list ms =
      (List.fold_left
         (fun acc ->
            fun m ->
              acc >>= (fun acc -> m >>= (fun x -> (x :: acc) |> return)))
         (return []) ms)
        >>= (fun xs -> (List.rev xs) |> return)
    let ( >>=* ) : 'x M.t list -> ('x list -> 'y M.t) -> 'y M.t =
      fun ms -> fun af -> (sequence_list ms) >>= af
  end
let foldl = List.fold_left[@@ocaml.doc " {!List.fold_right} "]
let foldr f z l = List.fold_right f l z[@@ocaml.doc
                                         " {!List.fold_left} but arg pos exchanged "]
let projected_compare proj a b = compare (proj a) (proj b)
module Either =
  struct
    type ('a, 'b) t =
      | Left of 'a 
      | Right of 'b 
    let left x = Left x
    let right x = Right x
  end
type ('a, 'b) either = ('a, 'b) Either.t
module Result =
  struct
    include Result
    let concat : ('x, 'e) result list -> ('x list, 'e) result =
      fun rs ->
        let rec loop acc =
          function
          | [] -> Ok acc
          | (Ok x)::rest -> loop (x :: acc) rest
          | (Error e)::_ -> Error e in
        loop [] (List.rev rs)[@@ocaml.doc
                               " NB - returning only the first error "]
  end
module ResultOf(E:sig type err end) =
  struct
    type err = E.err
    type 'x t = ('x, err) result
    let bind : 'x t -> ('x -> 'y t) -> 'y t = Result.bind
    let return : 'x -> 'x t = Result.ok
  end
module ResultWithErrmsg0 =
  struct
    include (ResultOf)(struct type err = string end)
    let protect' : handler:(exn -> string) -> ('x -> 'y) -> 'x -> 'y t =
      fun ~handler ->
        fun f -> fun x -> try Ok (f x) with | exn -> Error (handler exn)
    let protect : ('x -> 'y) -> 'x -> 'y t =
      fun f -> protect' ~handler:Printexc.to_string f
  end
module Queue :
  sig
    type 'x t
    val empty : 'x t
    val is_empty : 'x t -> bool
    val push : 'x -> 'x t -> 'x t
    val push_front : 'x -> 'x t -> 'x t
    val pop : 'x t -> ('x * 'x t) option
    val peek : 'x t -> ('x * 'x t) option
  end =
  struct
    type 'x t = ('x list * 'x list)
    let empty = ([], [])
    let is_empty = function | ([], []) -> true | _ -> false
    let push x (r, u) = ((x :: r), u)
    let push_front x (r, u) = (r, (x :: u))
    let rec pop (r, u) =
      match (u, r) with
      | (hd::rest, _) -> Some (hd, (r, rest))
      | ([], _::_) -> pop ([], (List.rev r))
      | ([], []) -> None
    let rec peek ((r, u) as q) =
      match (u, r) with
      | (hd::_, _) -> Some (hd, q)
      | ([], _::_) -> peek ([], (List.rev r))
      | ([], []) -> None
  end 
type 'x queue = 'x Queue.t
module Option0 =
  struct
    include Option
    let return = some
    let v default = function | Some x -> x | None -> default
    let v' gen_default = function | Some x -> x | None -> gen_default ()
    let otherwise otherwise = function | Some x -> Some x | None -> otherwise
    let pp vpp ppf =
      let open Format in
        function
        | Some x -> fprintf ppf "Some(%a)" vpp x
        | None -> fprintf ppf "None"
    let filter pred = function | Some x when pred x -> Some x | _ -> None
    let fmap f =
      function
      | None -> None
      | Some v -> (match f v with | None -> None | Some v -> v)
    let of_bool = function | true -> Some () | false -> None
    let some_if cond x = if cond then Some x else None
  end
module Option =
  struct
    include Option0
    module Ops_monad = (MonadOps)(Option0)
    module Ops = struct include Ops_monad end
  end
let some = Option.some
let none = Option.none
let (>?) o f = Option.map f o
let (>>?) o f = Option.bind o f
let (|?) o v = Option.v v o
let (||?) o1 o2 = Option.otherwise o2 o1
let (&>?) : ('x -> 'y option) -> ('y -> 'z) -> 'x -> 'z option =
  fun af -> fun f -> af &> (Option.map f)
module Seq0 =
  struct
    include Seq
    include (PipeOps)(Seq)
    let from : (unit -> 'x option) -> 'x t =
      fun f ->
        let rec next () =
          match f () with | None -> Nil | Some x -> Cons (x, next) in
        next
    let iota until_exclusive =
      let counter = ref 0 in
      from
        (fun () ->
           let x = !counter in
           if x = until_exclusive then None else (incr counter; Some x))
    let length s = fold_left (fun c -> fun _ -> succ c) 0 s
    let range ?include_endpoint:(ie= false)  start end_ =
      let end_exclusive = if ie then succ end_ else end_ in
      (iota (end_exclusive - start)) |&> ((+) start)
    let enum start =
      let counter = ref start in
      from (fun () -> (get_and_incr counter) |> Option.some)
    let rec limited quota orig () =
      if quota > 0
      then
        let open Seq in
          match orig () with
          | Nil -> Nil
          | Cons (x, next) -> Cons (x, (limited (pred quota) next))
      else Nil
    let iteri f s =
      let rec h i =
        function | Nil -> () | Cons (x, rest) -> (f i x; h (i + 1) (rest ())) in
      (s ()) |> (h 0)
    let hd s = match s () with | Nil -> raise Not_found | Cons (x, _) -> x
    let tl s = match s () with | Nil -> raise Not_found | Cons (_, t) -> t
    let take n s =
      match n with
      | _ when n < 0 -> failwith "panic"
      | _ ->
          let rec h n t () =
            match (n, (t ())) with
            | (0, _) -> Nil
            | (_, Nil) -> failwith "panic"
            | (_, Cons (x, u)) -> Cons (x, (h (n - 1) u)) in
          h n s
    let drop n s = Fn.ntimes n tl s
    let make n x =
      match n with
      | _ when n < 0 -> failwith "panic"
      | _ ->
          let rec h i () =
            match i with | 0 -> Nil | _ -> Cons (x, (h (i - 1))) in
          h n
  end
module Seq =
  struct
    include Seq0
    module Ops_piping = (PipeOps)(Seq0)
    module Ops = struct include Ops_piping end
  end
type 'x seq = 'x Seq.t
module Array0 =
  struct
    include Array
    let filter f arr = ((arr |> to_seq) |> (Seq.filter f)) |> of_seq
    let filter_map f arr = ((arr |> to_seq) |> (Seq.filter_map f)) |> of_seq
    include
      (PipeOps)(struct
                  include Array
                  let filter = filter
                  let filter_map = filter_map
                end)
    let of_list_of_length len list =
      let cell = ref list in
      init len
        (fun _ ->
           match !cell with
           | hd::tl -> (cell := tl; hd)
           | [] -> raise Not_found)
    let mean : ?f:('x -> float) -> float t -> float =
      fun ?(f= identity) ->
        fun arr ->
          let len = Array.length arr in
          if len = 0
          then raise Not_found
          else
            (let rec labor left right =
               match right - left with
               | 0 -> ((f (arr.(left))), 1)
               | 1 -> ((((f (arr.(left))) +. (f (arr.(right)))) /. 2.), 2)
               | rlen ->
                   if rlen < 0
                   then (0., 0)
                   else
                     (let mid = left + (rlen / 2) in
                      let (lv, lw) = labor left mid
                      and (rv, rw) = labor (succ mid) right in
                      let (!) = float_of_int in
                      ((((lv *. (!lw)) +. (rv *. (!rw))) /. (!(lw + rw))),
                        (lw + rw))) in
             (labor 0 (len - 1)) |> fst)
    let min cmp arr =
      match length arr with
      | 0 -> raise Not_found
      | _ ->
          let cand = ref (arr.(0)) in
          (iter (fun x -> if (cmp x (!cand)) < 0 then cand := x) arr; !cand)
    let max cmp arr =
      match length arr with
      | 0 -> raise Not_found
      | _ ->
          let cand = ref (arr.(0)) in
          (iter (fun x -> if (cmp x (!cand)) > 0 then cand := x) arr; !cand)
    let first arr =
      match length arr with | 0 -> raise Not_found | _ -> arr.(0)
    let last arr =
      match length arr with | 0 -> raise Not_found | n -> arr.(n - 1)
    let sorted cmp arr = sort cmp arr; arr
    let update : ('a -> 'a) -> 'a array -> int -> unit =
      fun f -> fun arr -> fun idx -> arr.(idx) <- (f (arr.(idx)))
    let update_each : (int -> 'a -> 'a) -> 'a array -> unit =
      fun f ->
        fun arr ->
          arr |> (Array.iteri (fun i -> fun x -> arr.(i) <- (f i x)))
    let blastsati : ('a -> bool) -> 'a array -> int =
      fun pred ->
        fun arr ->
          let pred i = pred (arr.(i)) in
          let rec loop pred l r =
            if l > r
            then raise Not_found
            else
              if (l + 1) = r
              then (if pred r then r else l)
              else
                if l = r
                then
                  (if (l = 0) && (not (pred l)) then raise Not_found else l)
                else
                  (let m = (l + r) / 2 in
                   if pred m then loop pred m r else loop pred l (m - 1)) in
          loop pred 0 ((length arr) - 1)
    let blastsat : ('a -> bool) -> 'a array -> 'a =
      fun pred -> fun arr -> (blastsati pred arr) |> (Array.get arr)[@@ocaml.doc
                                                                    " [blastsat] find the last element [e] such that\n      [pred e] being [true] using binary search.\n\n      more specifically,\n      - when [pred] yields [false] for every element, [Not_found] is raised\n      - when there exists [i >= 0] such that\n      {v     forall k <= i. (pred arr.(k)) = true\n/\\  forall k >  i, (pred arr.(k)) = false v}\n        , the [i]-th element will be returned\n      - otherwise, the behavior is undefined\n   "]
    let swap arr idx1 idx2 =
      let tmp = arr.(idx2) in arr.(idx2) <- (arr.(idx1)); arr.(idx1) <- tmp
    let shuffle : ?rng:(int -> int) -> 'x array -> unit =
      fun ?(rng= Random.int) ->
        fun arr ->
          let len = Array.length arr in
          for i = len - 1 downto 1 do swap arr i (rng (succ i)) done
    let to_function : 'a array -> int -> 'a = fun arr -> fun idx -> arr.(idx)
  end
module Array =
  struct
    include Array0
    module Ops_piping = (PipeOps)(Array0)
    module Ops_monad = (PipeOps)(Array0)
    module Ops = struct include Ops_piping
                        include Ops_monad end
  end
module Stream =
  struct
    include Stream
    let to_list_rev stream =
      let result = ref [] in
      Stream.iter (fun value -> result := (value :: (!result))) stream;
      !result
    let to_list stream = (to_list_rev stream) |> List.rev
    let hd stream =
      let open Stream in try next stream with | Failure -> raise Not_found
    let drop1 stream =
      let open Stream in
        let _ = try next stream with | Failure -> raise Not_found in stream
    let take n stream =
      let open Stream in
        let m_lst =
          try npeek n stream
          with | Failure -> raise Not_found | Error msg -> failwith msg in
        match List.length m_lst with
        | m when m = n -> m_lst
        | _ -> raise Not_found
    let drop n s = Fn.ntimes n drop1 s
  end
module List0 =
  struct
    include (PipeOps)(List)
    include List
    let iota = function | 0 -> [] | k -> 0 :: (List.init (pred k) succ)
    let range =
      let helper start end_ = (iota (end_ - start)) |&> ((+) start) in
      fun ?include_endpoint:(ie= false) ->
        if ie
        then fun start -> fun end_ -> helper start (succ end_)
        else (fun start -> fun end_ -> helper start end_)
    let dedup' ~by  l =
      let set = Hashtbl.create (List.length l) in
      l |?>
        (fun x ->
           if Hashtbl.mem set (by x)
           then false
           else (Hashtbl.add set (by x) true; true))
    let dedup l = dedup' ~by:identity l
    let update_assoc
      : 'k -> ('v option -> 'v option) -> ('k * 'v) list -> ('k * 'v) list =
      fun k ->
        fun func ->
          fun l ->
            let (l', updated) =
              l |>
                (fold_left
                   (fun (acc, updated) ->
                      fun ((key, v) as ent) ->
                        match (updated, (k = key)) with
                        | (false, true) ->
                            (match func (some v) with
                             | Some v' -> (((key, v') :: acc), true)
                             | None -> (acc, true))
                        | _ -> ((ent :: acc), updated)) ([], false)) in
            if not updated
            then match func none with | None -> l | Some v -> (k, v) :: l
            else rev l'
    let update_assq
      : 'k -> ('v option -> 'v option) -> ('k * 'v) list -> ('k * 'v) list =
      fun k ->
        fun func ->
          fun l ->
            let (l', updated) =
              l |>
                (fold_left
                   (fun (acc, updated) ->
                      fun ((key, v) as ent) ->
                        match (updated, (k == key)) with
                        | (false, true) ->
                            (match func (some v) with
                             | Some v' -> (((key, v') :: acc), true)
                             | None -> (acc, true))
                        | _ -> ((ent :: acc), updated)) ([], false)) in
            if not updated
            then match func none with | None -> l | Some v -> (k, v) :: l
            else rev l'
    let deassoc_opt : 'k -> ('k * 'v) list -> ('v option * ('k * 'v) list) =
      fun k ->
        fun es ->
          let rec loop (ret, es) =
            function
            | [] -> (ret, es)
            | (k', v)::rest when k' = k -> loop ((some v), es) rest
            | e::rest -> loop (ret, (e :: es)) rest in
          loop (none, []) es[@@ocaml.doc
                              " [deassoc_opt k l] removes entry keyed [k] from [l], interpreted as an association list,\n      and return [v, l'] where [v] is the value of the entry being removed or [None], and\n      [l'] is the list after the removal, or semantically unchanged if the key does not exist.\n      note that entries in [l'] may differ in order wrt. [l].\n\n      if there are multiple entries keyed [k], [v] will be [Some _] and [l'] will differ from the\n      original, but otherwise the behavior is unspecified "]
    let deassq_opt : 'k -> ('k * 'v) list -> ('v option * ('k * 'v) list) =
      fun k ->
        fun es ->
          let rec loop (ret, es) =
            function
            | [] -> (ret, es)
            | (k', v)::rest when k' == k -> loop ((some v), es) rest
            | e::rest -> loop (ret, (e :: es)) rest in
          loop (none, []) es[@@ocaml.doc
                              " same as [deassoc_opt] except using [(==)] when comparing keys "]
    let deassoc_opt' : 'k -> ('k * 'v) list -> ('v * ('k * 'v) list) option =
      fun k ->
        fun es ->
          match deassoc_opt k es with
          | (Some v, es) -> Some (v, es)
          | (None, _) -> None[@@ocaml.doc
                               " same as [deassoc_opt] but different return type "]
    let deassq_opt' : 'k -> ('k * 'v) list -> ('v * ('k * 'v) list) option =
      fun k ->
        fun es ->
          match deassq_opt k es with
          | (Some v, es) -> Some (v, es)
          | (None, _) -> None[@@ocaml.doc
                               " same as [deassq_opt] but different return type "]
    let deassoc : 'k -> ('k * 'v) list -> ('v * ('k * 'v) list) =
      fun k ->
        fun es ->
          let (ov, es) = deassoc_opt k es in
          ((Option.v' (fun () -> raise Not_found) ov), es)[@@ocaml.doc
                                                            " same as [deassoc_opt] but throws [Not_found] when the requested key does not exist "]
    let deassq : 'k -> ('k * 'v) list -> ('v * ('k * 'v) list) =
      fun k ->
        fun es ->
          let (ov, es) = deassq_opt k es in
          ((Option.v' (fun () -> raise Not_found) ov), es)[@@ocaml.doc
                                                            " same as [deassq_opt] but throws [Not_found] when the requested key does not exist "]
    let group_by : ('x -> 'k) -> 'x t -> ('k * 'x t) t =
      fun kf ->
        fun l ->
          l |>
            (fold_left
               (fun acc ->
                  fun x ->
                    let k = kf x in
                    update_assoc k
                      (function
                       | Some xs -> (x :: xs) |> some
                       | None -> some [x]) acc) [])
    let unzip l =
      List.fold_left (fun (l, s) -> fun (x, y) -> ((x :: l), (y :: s)))
        ([], []) (List.rev l)
    let unzip3 l =
      List.fold_left
        (fun (l1, l2, l3) ->
           fun (x1, x2, x3) -> ((x1 :: l1), (x2 :: l2), (x3 :: l3)))
        ([], [], []) (List.rev l)
    let reduce f = function | [] -> raise Not_found | hd::tl -> foldl f hd tl
    let min cmp =
      function
      | [] -> raise Not_found
      | hd::l ->
          let f acc x = if (cmp acc x) > 0 then x else acc in
          fold_left f hd l
    let max cmp =
      function
      | [] -> raise Not_found
      | hd::l ->
          let f acc x = if (cmp acc x) < 0 then x else acc in
          fold_left f hd l
    let foldl = foldl
    let foldr = foldr
    let hd = function | [] -> raise Not_found | h::_ -> h
    let tl = function | [] -> raise Not_found | _::tail -> tail
    let take n l =
      let rec loop acc =
        function
        | (0, _) -> rev acc
        | (n, hd::tl) -> loop (hd :: acc) ((n - 1), tl)
        | _ -> raise Not_found in
      loop [] (n, l)
    let drop n l = Fn.ntimes n tl l
    let make copies x = List.init copies (constant x)
    let count pred list =
      foldl (fun count -> fun x -> if pred x then succ count else count) 0
        list[@@ocaml.doc
              " [pred list] returns the number of elements [e] in [list] that satisfies [pred] "]
    let last list = foldl (fun _ -> fun x -> x) (List.hd list) list[@@ocaml.doc
                                                                    " last element of list "]
    let and_last : 'x . 'x list -> ('x list * 'x) =
      fun xs ->
        match rev xs with | [] -> raise Not_found | l::r -> ((rev r), l)
      [@@ocaml.doc " last element and rest of a list "]
    let fmap : ('x -> 'y list) -> 'x list -> 'y list =
      fun f ->
        fun l ->
          let rec loop acc =
            function
            | [] -> acc
            | x::r -> loop (((f x) |> List.rev) :: acc) r in
          let rec collect acc =
            function
            | [] -> acc
            | []::r' -> collect acc r'
            | (h::r)::r' -> collect (h :: acc) (r :: r') in
          (loop [] l) |> (collect [])
    let interpolate y xs =
      let rec loop acc =
        function
        | x::[] -> x :: acc
        | [] -> acc
        | x::xs -> loop (y :: x :: acc) xs in
      (loop [] xs) |> rev
    let filteri p l =
      let rec aux i acc =
        function
        | [] -> rev acc
        | x::l -> aux (i + 1) (if p i x then x :: acc else acc) l in
      aux 0 [] l
    let empty = function | [] -> true | _ -> false
    let to_function : 'a list -> int -> 'a =
      fun xs -> let open Array in (xs |> of_list) |> to_function
    let to_hashtbl : ('k * 'v) list -> ('k, 'v) Hashtbl.t =
      fun xs -> Hashtbl.of_seq (to_seq xs)
    let pp ?sep  ?parens  vpp ppf xs =
      let open Format in
        let (popen, pclose) =
          match parens with | Some parens -> parens | None -> ("[", "]") in
        let sep = match sep with | Some s -> s | None -> ";" in
        fprintf ppf "%s @[" popen;
        iter ((fprintf ppf "%a%s@;" vpp) |> (Fn.fix2nd sep)) xs;
        fprintf ppf "%s@]" pclose
    let bind ma af = fmap af ma
    let return x = [x]
  end
module List =
  struct
    include List0
    module Ops_piping = (PipeOps)(List0)
    module Ops_monad = (PipeOps)(List0)
    module Ops = struct include Ops_piping
                        include Ops_monad end
  end
include List.Ops_piping
let iota = List.iota
module Hashtbl =
  struct
    include Hashtbl
    let rev : ('a, 'b) t -> ('b, 'a) t =
      fun orig ->
        ((to_seq orig) |> (Seq.map (fun (k, v) -> (v, k)))) |> of_seq
      [@@ocaml.doc " swap the key and value "]
    let to_function : ('a, 'b) t -> 'a -> 'b = Hashtbl.find
    let make ?random  =
      (fun n ->
         fun genfunc ->
           let table = Hashtbl.create ?random n in
           ((Seq.iota n) |> (Seq.map genfunc)) |> (Hashtbl.add_seq table);
           table : int -> (int -> ('a * 'b)) -> ('a, 'b) Hashtbl.t)[@@ocaml.doc
                                                                    " [make n genfunc] creates a hashtable of [n] elements with entries\n      [{ (fst (genfunc 0))  |-> (snd (genfunc 0))\n       , (fst (genfunc 1))  |-> (snd (genfunc 1))\n         ...\n       , (fst (genfunc (n-1)))  |-> (snd (genfunc (n-1)))\n       }]  "]
  end
module String =
  struct
    include String
    let empty str = (length str) = 0[@@ocaml.doc
                                      " [empty str] returns true when str is of zero length "]
    let empty_trimmed str = (length (trim str)) = 0[@@ocaml.doc
                                                     " [empty_trimmed str] returns true when str is of zero length after being trimmed "]
    let chop_prefix prefix =
      let plen = length prefix in
      fun str ->
        let slen = length str in
        if slen < plen
        then None
        else
          if (sub str 0 plen) = prefix
          then Some (sub str plen (slen - plen))
          else None[@@ocaml.doc
                     " [chop_prefix p s] returns [s] minus the prefix [p] wrapped in [Some],\n      or [None] if [s] does not start with [p] "]
    let starts_with prefix str = (chop_prefix prefix str) |> Option.is_some
      [@@ocaml.doc
        " [starts_with p s] returns whether [s] starts with a substring of [p] "]
    let ends_with postfix str =
      let (plen, slen) = ((length postfix), (length str)) in
      if slen < plen then false else (sub str (slen - plen) plen) = postfix
      [@@ocaml.doc
        " [ends_with p s] returns whether [s] ends with a substring of [p] "]
    let chop_suffix suffix =
      let plen = length suffix in
      fun str ->
        let slen = length str in
        if slen < plen
        then None
        else
          if (sub str (slen - plen) plen) = suffix
          then Some (sub str 0 (slen - plen))
          else None[@@ocaml.doc
                     " [chop_prefix p s] returns [s] minus the suffix [p] wrapped in [Some],\n      or [None] if [s] does not end with [p] "]
    let to_bytes = Bytes.of_string
    let to_list str = (to_seq str) |> List.of_seq
    let of_list = List.to_seq &> of_seq
    let of_array = Array.to_seq &> of_seq
  end
module IoPervasives =
  struct
    let with_input_file path f =
      let ch = open_in path in
      let r = try f ch with | e -> (close_in ch; raise e) in close_in ch; r
    let with_output_file path f =
      let ch = open_out path in
      let r = try f ch with | e -> (close_out ch; raise e) in close_out ch; r
    let slurp_input ?buf  ic =
      let buf =
        match buf with | None -> Bytes.make 4096 '\000' | Some buf -> buf in
      let result = ref "" in
      let rec loop len =
        match input ic buf 0 len with
        | 0 -> result
        | rlen ->
            (result := ((!result) ^ (Bytes.sub_string buf 0 rlen)); loop len) in
      !(loop (Bytes.length buf))
    let slurp_stdin ?buf  () = slurp_input ?buf stdin
    let slurp_file path = with_input_file path slurp_input[@@warning "-48"]
    let spit_file path str =
      with_output_file path (Fn.flip output_string str)
  end
include IoPervasives
module Timing =
  struct
    let timefunc' output f =
      let t = Sys.time () in
      let r = f () in output := ((Sys.time ()) -. t); r[@@ocaml.doc
                                                         " time the execution of [f], returning the result\n        of [f] and store the measured time in [output] "]
    let timefunc f = let time = ref 0. in (timefunc' time f) |> ignore; !time
      [@@ocaml.doc
        " time the execution of [f], discarding the result of [f] "]
  end
module Datetime0 :
  sig
    [@@@ocaml.text " all according to proleptic Gregorian Calender "]
    val leap_year : int -> bool
    val daycount_of_month : leap:bool -> int -> int
    val day_of_year : int -> int -> int -> int
    module type NormalizedTimestamp  =
      sig
        [@@@ocaml.text " timezone not taking into consideration "]
        module Conf :
        sig
          val epoch_year : int[@@ocaml.doc
                                " the epoch would be at January 1st 00:00:00.0 in [epoch_year] "]
          val subsecond_resolution : int[@@ocaml.doc
                                          " e.g. sec-resolution use [1] and millisec-resolution use [1000] "]
          val min_year : int[@@ocaml.doc " min-year supported "]
          val max_year : int[@@ocaml.doc " max-year supported "]
        end
        val normalize :
          ?subsec:int ->
            ?tzoffset:(int * int) ->
              (int * int * int) -> (int * int * int) -> int[@@ocaml.doc
                                                             " [normalize\n         ?tzoffset:(tzhour, tzmin)\n         ?subsec (yy, mm, dd) (hour, min, sec)]\n        calculates the normalized timestamp "]
      end
    module EpochNormalizedTimestamp :
    functor (Conf :
      sig
        [@@@ocaml.text " see NormalizedTimestamp "]
        val epoch_year : int
        val subsecond_resolution : int
      end) -> NormalizedTimestamp
    module UnixTimestmapSecRes : NormalizedTimestamp
    module UnixTimestmapMilliRes : NormalizedTimestamp
    module UnixTimestmapNanoRes : NormalizedTimestamp
  end =
  struct
    let sum = List.foldl (+) 0
    let days_of_months_nonleap =
      List.to_function @@ [0; 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]
    let days_of_months_leap =
      List.to_function @@ [0; 31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]
    let days_of_months_subsum_nonleap =
      let open List in
        ((iota 13) |&>
           (fun x -> ((iota x) |&> days_of_months_nonleap) |> sum))
          |> to_function
    let days_of_months_subsum_leap =
      let sum = List.foldl (+) 0 in
      let open List in
        ((iota 13) |&> (fun x -> ((iota x) |&> days_of_months_leap) |> sum))
          |> to_function
    let daycount_of_month ~leap  =
      let table =
        if leap then days_of_months_leap else days_of_months_nonleap in
      fun mm -> table mm
    let leap_year yy =
      let div x = (yy mod x) = 0 in
      if not (div 4)
      then false
      else if not (div 100) then true else if div 400 then true else false
    let day_of_year yy =
      let table =
        match leap_year yy with
        | false -> days_of_months_subsum_nonleap
        | true -> days_of_months_subsum_leap in
      fun mm -> fun dd -> (table mm) + dd
    module type NormalizedTimestamp  =
      sig
        module Conf :
        sig
          val epoch_year : int
          val subsecond_resolution : int
          val min_year : int
          val max_year : int
        end
        val normalize :
          ?subsec:int ->
            ?tzoffset:(int * int) ->
              (int * int * int) -> (int * int * int) -> int[@@ocaml.doc
                                                             " [normalize yy mm dd ?subsec hour min sec] calculates the normalized timestamp "]
      end
    module EpochNormalizedTimestamp(Conf:sig
                                           val epoch_year : int
                                           val subsecond_resolution : int
                                         end) =
      struct
        module Conf =
          struct
            include Conf
            let min_year = Conf.epoch_year
            let max_year =
              let span =
                (pred Int.max_int) /
                  ((((366 * 24) * 60) * 60) * subsecond_resolution) in
              (span - 1) + min_year
          end
        open Conf
        let yearcount_leaping ymin ymax =
          let roundup div x =
            if (x mod div) = 0 then x else div * (succ (x / div)) in
          let ncat div =
            let span = ymax - (roundup div ymin) in
            if span < 0 then 0 else succ (span / div) in
          let ncat4 = ncat 4 in
          let ncat100 = ncat 100 in
          let ncat400 = ncat 400 in (ncat4 - ncat100) + ncat400
        let normalize ?subsec  ?tzoffset  (yy, mm, dd) (hour, min, sec) =
          let subsec = let open Option in value ~default:0 subsec in
          if (yy < min_year) || (yy > max_year)
          then
            invalid_arg
              (let open Format in
                 asprintf
                   "%s.normalize - timestamp cannot be handled: %d-%d-%d %02d:%02d:%02d (subsec: %d/%d) - year out of range (%d-%d)"
                   "/kxclib.ml/.Datetime0.EpochNormalizedTimestamp" yy mm dd
                   hour min sec subsec subsecond_resolution min_year max_year);
          if subsec >= subsecond_resolution
          then
            invalid_arg
              (let open Format in
                 sprintf "%s.normalize - subsec out of range (%d-%d)"
                   "/kxclib.ml/.Datetime0.EpochNormalizedTimestamp" 0
                   (pred subsecond_resolution));
          (let days_past_years =
             let (ymin, ymax) = (epoch_year, (pred yy)) in
             let leaping = yearcount_leaping ymin ymax in
             let nonleaping = ((ymax - ymin) + 1) - leaping in
             (leaping * 366) + (nonleaping * 365) in
           let doy = day_of_year yy mm dd in
           let (hour, min) =
             match tzoffset with
             | None -> (hour, min)
             | Some (tzhour, tzmin) -> ((hour + tzhour), (min + tzmin)) in
           let nts =
             ((sec + (min * 60)) + ((hour * 60) * 60)) +
               ((((days_past_years + doy) * 24) * 60) * 60) in
           let nts = (nts * subsecond_resolution) + subsec in nts)
      end
    module UnixTimestmapSecRes =
      (EpochNormalizedTimestamp)(struct
                                   let epoch_year = 1970
                                   let subsecond_resolution = 1
                                 end)
    module UnixTimestmapMilliRes =
      (EpochNormalizedTimestamp)(struct
                                   let epoch_year = 1970
                                   let subsecond_resolution = 1000
                                 end)
    module UnixTimestmapNanoRes =
      (EpochNormalizedTimestamp)(struct
                                   let epoch_year = 1970
                                   let subsecond_resolution =
                                     (1000 * 1000) * 1000
                                 end)
  end 
module ParseArgs =
  struct
    type optparser = string -> [ `Process_next of bool ]
    let prefset r x = r := x
    let prefsetv r v _ = r := v
    let scanfparser fmt fn =
      (fun str ->
         Scanf.ksscanf str (fun _ -> fun _ -> `Process_next true) fmt fn;
         `Process_next false : optparser)
    let exactparser fmt (fn : unit -> unit) =
      (function
       | str when str = fmt -> (fn (); `Process_next false)
       | _ -> `Process_next true : optparser)
    let parse_opts (optparsers : optparser list) ?(argsource= (Sys.argv, 1)) 
      () =
      let rec tryparse str =
        function
        | [] -> raise (Invalid_argument ("unparsed option: " ^ str))
        | p::ps ->
            (match (p : optparser) str with
             | `Process_next (true) -> tryparse str ps
             | `Process_next (false) -> ()) in
      ((Array.to_list (fst argsource)) |> (List.drop (snd argsource))) |!>
        (Fn.fix2nd optparsers tryparse)
    let parse_opts_args ?(optprefix= "-")  ?(optsep= "--") 
      (optparsers : optparser list) ?(argsource= (Sys.argv, 1))  () =
      let (source, startidx) = argsource in
      let optprefixlen = String.length optprefix in
      let prefixed str =
        if (String.length str) < optprefixlen
        then false
        else (String.sub str 0 optprefixlen) = optprefix in
      let argc = Array.length source in
      let args = ref [] in
      let rec tryparse str =
        function
        | [] -> raise (Invalid_argument ("unparsed option: " ^ str))
        | p::ps ->
            (match p str with
             | `Process_next (true) -> tryparse str ps
             | `Process_next (false) -> ()) in
      let tryparse = Fn.fix2nd optparsers tryparse in
      let rec loop n parseopt =
        if n >= argc
        then List.rev (!args)
        else
          (let arg = source.(n) in
           if not parseopt
           then (refappend args arg; loop (succ n) parseopt)
           else
             if arg = optsep
             then loop (succ n) false
             else
               if prefixed arg
               then (tryparse arg; loop (succ n) parseopt)
               else (refappend args arg; loop (succ n) parseopt)) in
      loop startidx true
  end
module ArgOptions =
  struct
    type _ named_option =
      | IntOption: string -> int named_option 
      | FloatOption: string -> float named_option 
      | StringOption: string -> string named_option 
      | InChannelOption: string -> in_channel named_option 
      | OutChannelOption: string -> out_channel named_option 
      | InChannelOption': string -> (in_channel * channel_desc) named_option
      
      | OutChannelOption': string -> (out_channel * channel_desc)
      named_option 
    and channel_desc = [ `StandardChannel  | `FileChannel of string ]
    let opt_of_named_option (type x) (opt : x named_option) =
      match opt with
      | IntOption opt -> opt
      | FloatOption opt -> opt
      | StringOption opt -> opt
      | InChannelOption opt -> opt
      | OutChannelOption opt -> opt
      | InChannelOption' opt -> opt
      | OutChannelOption' opt -> opt
    module type FeatureRequests  =
      sig
        val has_flag :
          ?argsource:(string array * int) ->
            ?prefix:string -> ((string)[@ocaml.doc " flag "]) -> bool
        val get_option :
          ?argsource:(string array * int) ->
            ?optprefix:string -> ?optsep:string -> 'x named_option -> 'x
        val get_option_d :
          ?argsource:(string array * int) ->
            ?optprefix:string ->
              ?optsep:string ->
                'x named_option -> (('x)[@ocaml.doc " default value "]) -> 'x
        val get_option_d' :
          ?argsource:(string array * int) ->
            ?optprefix:string ->
              ?optsep:string ->
                'x named_option ->
                  ((unit -> 'x)[@ocaml.doc " default value producer "]) -> 'x
        val get_args :
          ?argsource:(string array * int) ->
            ?optsep:string -> unit -> string list
      end
    let has_flag ?argsource  ?(prefix= "")  flag =
      let store = ref false in
      ((let open ParseArgs in
          parse_opts
            [exactparser (prefix ^ flag) (fun () -> store := true);
            constant (`Process_next false)])) ?argsource ();
      !store
    let get_option ?argsource  ?(prefix= "")  ?optsep  (type x) =
      (let open ParseArgs in
         let labor opt f =
           let state = ref `Init in
           let result = ref None in
           let marker_raw = prefix ^ opt in
           let marker_eq = marker_raw ^ "=" in
           let par arg =
             match !state with
             | `Init when arg = marker_raw ->
                 (state := `CaptureNext; `Process_next true)
             | `Init ->
                 (match String.chop_prefix marker_eq arg with
                  | Some arg ->
                      (result := (Some (f arg)); `Process_next false)
                  | None -> `Process_next true)
             | `CaptureNext ->
                 ((state := `Init; result := (Some (f arg)));
                  `Process_next false) in
           (parse_opts_args ?argsource ~optprefix:"" ?optsep
              [par; constant (`Process_next false)] ())
             |> ignore;
           (match !state with
            | `Init -> !result
            | `CaptureNext ->
                invalid_arg ("no argument supplied to option " ^ opt)) in
         function
         | IntOption opt ->
             labor opt (fun arg -> Scanf.sscanf arg "%i%!" identity)
         | FloatOption opt ->
             labor opt (fun arg -> Scanf.sscanf arg "%g%!" identity)
         | StringOption opt -> labor opt identity
         | InChannelOption opt ->
             labor opt (function | "-" -> stdin | path -> open_in path)
         | OutChannelOption opt ->
             labor opt (function | "-" -> stdout | path -> open_out path)
         | InChannelOption' opt ->
             labor opt
               (function
                | "-" -> (stdin, `StandardChannel)
                | path -> ((open_in path), (`FileChannel path)))
         | OutChannelOption' opt ->
             labor opt
               (function
                | "-" -> (stdout, `StandardChannel)
                | path -> ((open_out path), (`FileChannel path))) : x
                                                                    named_option
                                                                    ->
                                                                    x option)
    let get_option_exn ?argsource  ?prefix  ?optsep  (type x) =
      (fun opt ->
         match get_option ?argsource ?prefix ?optsep opt with
         | None ->
             invalid_arg
               ("you have to provide option " ^ (opt_of_named_option opt))
         | Some x -> x : x named_option -> x)
    let get_option_d' ?argsource  ?prefix  ?optsep  (type x) =
      (fun opt ->
         fun vp ->
           match get_option ?argsource ?prefix ?optsep opt with
           | None -> vp ()
           | Some x -> x : x named_option -> (unit -> x) -> x)
    let get_option_d ?argsource  ?prefix  ?optsep  (type x) =
      (fun opt ->
         fun v ->
           match get_option ?argsource ?prefix ?optsep opt with
           | None -> v
           | Some x -> x : x named_option -> x -> x)
    let get_absolute_args ?(optsep= "--")  ?(argsource= (Sys.argv, 1))  () =
      let (source, startidx) = argsource in
      let argc = Array.length source in
      let args = ref [] in
      let rec loop n record_arg =
        if n >= argc
        then List.rev (!args)
        else
          (let arg = source.(n) in
           if record_arg
           then (refappend args arg; loop (succ n) record_arg)
           else
             if arg = optsep
             then loop (succ n) true
             else loop (succ n) record_arg) in
      loop startidx false
  end
module FmtPervasives =
  struct
    type ppf = Format.formatter
    let color_enabled = ref true
    let fprintf ppf fmt = Format.fprintf ppf fmt
    let printf fmt = Format.printf fmt
    let sprintf fmt = Format.asprintf fmt
    let eprintf fmt = Format.eprintf fmt
    module Fmt =
      struct
        let stdout_ppf = Format.std_formatter
        let stderr_ppf = Format.err_formatter
        let null_ppf =
          Format.formatter_of_out_functions
            {
              out_string = (fun _ -> fun _ -> fun _ -> ());
              out_flush = (fun _ -> ());
              out_newline = (fun _ -> ());
              out_spaces = (fun _ -> ());
              out_indent = (fun _ -> ())
            }
        let colored ?style  ?color_mode:(m= `Fg)  color ppf fmt =
          if !color_enabled
          then
            let code_table =
              function
              | `Black -> (30, 40)
              | `Red -> (31, 41)
              | `Green -> (32, 42)
              | `Yellow -> (33, 43)
              | `Blue -> (34, 44)
              | `Magenta -> (35, 45)
              | `Cyan -> (36, 46)
              | `White -> (37, 47)
              | `Bright_black -> (90, 100)
              | `Bright_red -> (91, 101)
              | `Bright_green -> (92, 102)
              | `Bright_yellow -> (93, 103)
              | `Bright_blue -> (94, 104)
              | `Bright_magenta -> (95, 105)
              | `Bright_cyan -> (96, 106) in
            let style_table =
              function
              | `Bold -> 1
              | `Thin -> 2
              | `Italic -> 3
              | `Underline -> 4 in
            let esc x = "\027" ^ x in
            let reset = "[0m" in
            let color_code =
              ((code_table color) |> (match m with | `Fg -> fst | `Bg -> snd))
                |> (sprintf "[%dm") in
            let style_code =
              style |>
                (function
                 | None -> None
                 | Some s ->
                     ((style_table s) |> (sprintf "[%dm")) |> Option.some) in
            (Format.fprintf ppf "@<0>%s"
               ((esc color_code) ^ ((style_code |> (Option.map esc)) |? ""));
             Format.kfprintf
               (fun ppf -> Format.fprintf ppf "@<0>%s" (esc reset)) ppf fmt)
          else Format.fprintf ppf fmt
      end
    let condformat cond fmtfunc fmt =
      if cond then fmtfunc fmt else Format.ifprintf Fmt.null_ppf fmt
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
      fprintf ppf "%#x" (2 * (Obj.magic r))
    let pp_int32 ppf x = (Int32.to_string x) |> (pp_string ppf)
    let pp_int64 ppf x = (Int64.to_string x) |> (pp_string ppf)
    let pp_integer_sep' ~padding  ppf x =
      let rec loop acc x =
        if x > 0 then loop ((x mod 1000) :: acc) (x / 1000) else acc in
      let chunks = loop [] (abs x) in
      let chunks =
        match chunks with
        | x::[] -> [string_of_int x]
        | h::r -> (string_of_int h) :: (r |&> (sprintf "%03d"))
        | [] -> ["0"] in
      if x < 0 then pp_char ppf '-';
      (let str = String.concat "," chunks in
       (match padding with
        | None -> ()
        | Some (0, _) -> ()
        | Some (d, pad) ->
            let d =
              (d + ((Float.ceil ((float_of_int d) /. 3.)) |> int_of_float)) -
                1 in
            let slen = String.length str in
            if d > slen
            then Fn.ntimes (d - slen) (fun () -> pp_char ppf pad) ());
       pp_string ppf str)[@@ocaml.doc
                           " print integer with thousand separator "]
    let pp_integer_sep ppf = pp_integer_sep' ~padding:None ppf
    let pp_multiline ppf str =
      let open Format in
        let rec loop =
          function
          | line::[] -> pp_string ppf line
          | line::rest ->
              (pp_string ppf line; pp_force_newline ppf (); loop rest)
          | [] -> () in
        (String.split_on_char '\n' str) |> loop
    let pp_exn ppf exn =
      (Printexc.to_string exn) |> (Format.pp_print_string ppf)
    let pp_full_exn' ppf (exn, bt) =
      Format.fprintf ppf "@<2>%s@[<hov>@\n%a@]" (Printexc.to_string exn)
        pp_multiline (let open Printexc in bt |> raw_backtrace_to_string)
    let pp_full_exn ppf exn =
      pp_full_exn' ppf (exn, (let open Printexc in get_raw_backtrace ()))
    let string_of_symbolic_output_items
      : Format.symbolic_output_item list -> string =
      fun items ->
        let buf = Buffer.create 0 in
        items |!>
          ((function
            | Output_flush -> ()
            | Output_newline -> Buffer.add_char buf '\n'
            | Output_string str -> Buffer.add_string buf str
            | Output_spaces n | Output_indent n ->
                Buffer.add_string buf (String.make n ' ')));
        Buffer.contents buf
  end
include FmtPervasives
module Log0 =
  struct
    open Format
    module Internals =
      struct
        let timestamp_func = ref (constant None)
        let logging_formatter = ref err_formatter
      end
    open Internals
    module LoggingConfig =
      struct
        let install_timestamp_function func = timestamp_func := func
        let set_logging_formatter ppf = logging_formatter := ppf
        let get_logging_formatter () = !logging_formatter
      end
    let logr fmt = fprintf (!logging_formatter) fmt
    let log ~label  ?modul  ?header_style:(style= None) 
      ?header_color:(color= `Magenta)  fmt =
      let header =
        match modul with | None -> label | Some m -> label ^ (":" ^ m) in
      let header =
        match (!timestamp_func) () with
        | None -> sprintf "[%s]" header
        | Some ts -> sprintf "[%s :%.3f]" header ts in
      let pp_header ppf = Fmt.colored ?style color ppf "%s" in
      logr "@<1>%s @[<hov>" (asprintf "%a" pp_header header);
      kfprintf (fun ppf -> fprintf ppf "@]@.") (!logging_formatter) fmt
    let verbose ?modul  fmt =
      log ?modul fmt ~label:"VERBOSE" ~header_style:(Some `Thin)
        ~header_color:`Bright_cyan
    let info ?modul  fmt =
      log ?modul fmt ~label:"INFO" ~header_style:(Some `Bold)
        ~header_color:`Bright_cyan
    let warn ?modul  fmt =
      log ?modul fmt ~label:"WARN" ~header_style:(Some `Bold)
        ~header_color:`Yellow
    let debug ?modul  fmt =
      log ?modul fmt ~label:"DEBUG" ~header_style:(Some `Bold)
        ~header_color:`Magenta
    let error ?modul  fmt =
      log ?modul fmt ~label:"ERROR" ~header_style:(Some `Bold)
        ~header_color:`Red
    module Pervasives =
      struct
        let debug ?modul  fmt = debug ?modul fmt
        let info ?modul  fmt = info ?modul fmt
      end
  end
include Log0.Pervasives
module Json :
  sig
    type jv =
      [ `null  | `bool of bool  | `num of float  | `str of string 
      | `arr of jv list  | `obj of (string * jv) list ]
    type jv_field = (string * jv)
    type jv_fields = jv_field list
    type jvpath =
      ([ `f of string [@ocaml.doc " field within an object "]
       | `i of int [@ocaml.doc " index within an array "]] as 'path_component)
        list[@@ocaml.doc " an empty path designate the root element "]
    type legacy = [ `arr of jv list  | `obj of (string * jv) list ]
    val of_legacy : legacy -> jv
    val to_legacy : jv -> legacy option
    type yojson =
      [ `Null  | `Bool of bool  | `Int of int  | `Intlit of string 
      | `Float of float  | `String of string  | `Assoc of (string * 't) list 
      | `List of 't list  | `Tuple of 't list 
      | `Variant of (string * 't option) ] as 't[@@ocaml.doc
                                                  " Yojson.Safe.t "]
    val of_yojson : yojson -> jv
    val to_yojson : jv -> yojson
    type yojson' =
      [ `Null  | `Bool of bool  | `Int of int  | `Float of float 
      | `String of string  | `Assoc of (string * 't) list 
      | `List of 't list ] as 't[@@ocaml.doc " Yojson.Basic.t "]
    val yojson_basic_of_safe : yojson -> yojson'
    val yojson_safe_of_basic : yojson' -> yojson
    type jsonm = jsonm_token seq
    and jsonm_token =
      [ `Null  | `Bool of bool  | `String of string  | `Float of float 
      | `Name of string  | `As  | `Ae  | `Os  | `Oe ]
    type 'loc jsonm' = ('loc * jsonm_token) seq
    type 'loc jsonm_pe =
      [ `empty_document 
      | `premature_end of 'loc
          [@ocaml.doc
            " with loc of the starting token of the inner-most structure (viz. array/object) "]
      | `expecting_value_at of 'loc 
      | `unexpected_token_at of ('loc * jsonm_token) ]
    val of_jsonm' : 'loc jsonm' -> ((jv * 'loc jsonm'), 'loc jsonm_pe) result
    val of_jsonm : jsonm -> (jv * jsonm) option
    val to_jsonm : jv -> jsonm
  end =
  struct
    type jv =
      [ `null  | `bool of bool  | `num of float  | `str of string 
      | `arr of jv list  | `obj of (string * jv) list ]
    type jv_field = (string * jv)
    type jv_fields = jv_field list
    type jvpath = ([ `f of string  | `i of int ] as 'path_component) list
    type legacy = [ `arr of jv list  | `obj of (string * jv) list ]
    type yojson =
      [ `Null  | `Bool of bool  | `Int of int  | `Intlit of string 
      | `Float of float  | `String of string  | `Assoc of (string * 't) list 
      | `List of 't list  | `Tuple of 't list 
      | `Variant of (string * 't option) ] as 't
    let of_legacy x = (x :> jv)
    let to_legacy : jv -> legacy option =
      function | #legacy as x -> Some x | _ -> None
    let rec of_yojson : yojson -> jv =
      function
      | `Null -> `null
      | `Bool x -> `bool x
      | `Int x -> `num (float_of_int x)
      | `Intlit x -> `num (float_of_string x)
      | `Float x -> `num x
      | `String x -> `str x
      | `Assoc x -> `obj (x |&> (?> of_yojson))
      | `List x -> `arr (x |&> of_yojson)
      | `Tuple x -> `arr (x |&> of_yojson)
      | `Variant (t, Some x) -> `arr [`str t; of_yojson x]
      | `Variant (t, None) -> `str t
    let rec to_yojson : jv -> yojson =
      function
      | `null -> `Null
      | `bool x -> `Bool x
      | `num x ->
          if
            (Float.is_integer x) &&
              ((x <= (Int.max_int |> float_of_int)) &&
                 (x >= (Int.min_int |> float_of_int)))
          then `Int (Float.to_int x)
          else `Float x
      | `str x -> `String x
      | `arr x -> `List (x |&> to_yojson)
      | `obj x -> `Assoc (x |&> (?> to_yojson))
    type yojson' =
      [ `Null  | `Bool of bool  | `Int of int  | `Float of float 
      | `String of string  | `Assoc of (string * 't) list 
      | `List of 't list ] as 't
    let rec yojson_basic_of_safe : yojson -> yojson' =
      fun yojson ->
        match yojson with
        | `Null -> `Null
        | `Bool x -> `Bool x
        | `Int x -> `Int x
        | `Intlit x -> `Int (int_of_string x)
        | `Float x -> `Float x
        | `String x -> `String x
        | `Assoc xs ->
            `Assoc (xs |&> ((fun (n, x) -> (n, (yojson_basic_of_safe x)))))
        | `List xs -> `List (xs |&> yojson_basic_of_safe)
        | `Tuple xs -> `List (xs |&> yojson_basic_of_safe)
        | `Variant (c, x_opt) ->
            (match Option.map yojson_basic_of_safe x_opt with
             | None -> `List [`String c]
             | Some x -> `List [`String c; x])
    let yojson_safe_of_basic : yojson' -> yojson = fun x -> (x :> yojson)
    type jsonm = jsonm_token seq
    and jsonm_token =
      [ `Null  | `Bool of bool  | `String of string  | `Float of float 
      | `Name of string  | `As  | `Ae  | `Os  | `Oe ]
    type atomic_jsonm_token =
      [ `Null  | `Bool of bool  | `String of string  | `Float of float ]
    type value_starting_jsonm_token = [ | atomic_jsonm_token | `As  | `Os ]
    type 'loc jsonm' = ('loc * jsonm_token) seq
    type 'loc jsonm_pe =
      [ `empty_document 
      | `premature_end of 'loc
          [@ocaml.doc
            " with loc of the starting token of the inner-most structure (viz. array/object) "]
      | `expecting_value_at of 'loc 
      | `unexpected_token_at of ('loc * jsonm_token) ]
    let of_jsonm' : 'loc jsonm' -> ((jv * 'loc jsonm'), 'loc jsonm_pe) result
      =
      fun input ->
        let (>>=) m f = Result.bind m f in
        let jv_of_atom : atomic_jsonm_token -> jv =
          function
          | `Null -> `null
          | `Bool x -> `bool x
          | `String x -> `str x
          | `Float x -> `num x
          | _ -> . in
        let with_next (sloc : 'loc) (next : 'loc jsonm')
          (kont : 'loc -> 'loc jsonm' -> jsonm_token -> 'r) =
          (match next () with
           | Seq.Nil -> Error (`premature_end sloc)
           | Seq.Cons ((nloc, ntok), next') -> kont nloc next' ntok : 
          'r) in
        let ok next x =
          (Ok (x, next) : ((jv * 'loc jsonm'), 'loc jsonm_pe) result) in
        let rec value loc next =
          function
          | #atomic_jsonm_token as tok -> (jv_of_atom tok) |> (ok next)
          | `As -> with_next loc next (collect_array [])
          | `Os -> with_next loc next (collect_object [])
          | #value_starting_jsonm_token -> .
          | `Name _ | `Ae | `Oe as tok ->
              Error (`unexpected_token_at (loc, tok))
          | _ -> .
        and collect_array acc sloc next =
          function
          | `Ae -> ok next (`arr (List.rev acc))
          | #value_starting_jsonm_token ->
              (with_next sloc next value) >>=
                ((fun (v, next) ->
                    with_next sloc next
                      (fun _nloc -> collect_array (v :: acc) sloc)))
          | `Name _ | `Oe as tok -> Error (`unexpected_token_at (sloc, tok))
          | _ -> .
        and collect_object acc sloc next =
          function
          | `Oe -> ok next (`obj (List.rev acc))
          | `Name key ->
              (with_next sloc next value) >>=
                ((fun (v, next) ->
                    with_next sloc next
                      (fun _nloc -> collect_object ((key, v) :: acc) sloc)))
          | #value_starting_jsonm_token | `Ae as tok ->
              Error (`unexpected_token_at (sloc, tok))
          | _ -> . in
        match input () with
        | Seq.Nil -> Error `empty_document
        | Seq.Cons ((loc, tok), next) -> value loc next tok
    let of_jsonm : jsonm -> (jv * jsonm) option =
      fun jsonm ->
        (((Seq.map (fun tok -> ((), tok)) jsonm) |> of_jsonm') |>
           Result.to_option)
          |> (Option.map (fun (out, rest) -> (out, (Seq.map snd rest))))
    let rec to_jsonm : jv -> jsonm =
      function
      | `null -> Seq.return `Null
      | `bool x -> Seq.return (`Bool x)
      | `num x -> Seq.return (`Float x)
      | `str x -> Seq.return (`String x)
      | `arr xs ->
          Seq.cons `As
            (List.fold_right
               (fun x -> fun seq -> Seq.append (to_jsonm x) seq) xs
               (Seq.return `Ae))
      | `obj xs ->
          Seq.cons `Os
            (List.fold_right
               (fun (name, x) ->
                  fun seq ->
                    Seq.append (Seq.cons (`Name name) (to_jsonm x)) seq) xs
               (Seq.return `Oe))
  end 
module Jv =
  struct
    open Json
    let pump_field fname =
      (function
       | `obj ((_, _)::[]) as jv -> jv
       | `obj fs as jv ->
           (match List.deassoc_opt fname fs with
            | (Some fval, fs') -> `obj ((fname, fval) :: fs')
            | (None, _) -> jv)
       | jv -> jv : jv -> jv)
  end
module Base64 =
  struct
    module type Config  =
      sig
        val c62 : char[@@ocaml.doc
                        " the 62nd character. ['+'] in rfc4648, ['-'] in rfc4648_url. "]
        val c63 : char[@@ocaml.doc
                        " the 63rd character. ['/'] in rfc4648, ['_'] in rfc4648_url. "]
        val pad : char option[@@ocaml.doc
                               " the pad character. if [None], padding is disabled.\n\n        [Some '='] in rfc4648. [None] in rfc4648_url. "]
        val ignore_newline : bool[@@ocaml.doc
                                   " if set to true, newline characters are ignored on decoding. "]
        val ignore_unknown : bool[@@ocaml.doc
                                   " if set to true, unknown characters are ignored on decoding.\n\n        [ignore_unknown = true] implies [ignore_newline = true]. "]
      end
    module type T  =
      sig
        val encode_buf : ?offset:int -> ?len:int -> Buffer.t -> bytes -> int
        [@@ocaml.doc
          "\n      Takes an input [bytes], and writes the encoded string to [Buffer.t].\n      @param offset   the offset of input which the encoder should start reading from.\n      @param len      the length of input which the encoder should read.\n      @return the number of bytes written to [Buffer.t].\n    "]
        val decode_buf : ?offset:int -> ?len:int -> Buffer.t -> string -> int
        [@@ocaml.doc
          "\n      Takes an input [string], and writes the decoded bytes to [Buffer.t].\n      @param offset   the offset of input which the decoder should start reading from.\n      @param len      the length of input which the decoder should read.\n      @return the number of bytes written to [Buffer.t].\n    "]
        val encode : ?offset:int -> ?len:int -> bytes -> string[@@ocaml.doc
                                                                 "\n      Takes an input [bytes], and returns the encoded [string].\n      @param offset   the offset of input which the encoder should start reading from.\n      @param len      the length of input which the encoder should read.\n    "]
        val decode : ?offset:int -> ?len:int -> string -> bytes[@@ocaml.doc
                                                                 "\n      Takes an input [string], and returns the decoded [bytes].\n      @param offset   the offset of input which the decoder should start reading from.\n      @param len      the length of input which the decoder should read.\n    "]
      end
    module Make(C:Config) : T =
      struct
        open C
        let int_A = int_of_char 'A'
        let int_Z = int_of_char 'Z'
        let int_a = int_of_char 'a'
        let int_z = int_of_char 'z'
        let int_0 = int_of_char '0'
        let int_9 = int_of_char '9'
        let (c62, c63) = ((int_of_char c62), (int_of_char c63))
        let sixbit_to_char b =
          if b < 26
          then b + int_A
          else
            if b < 52
            then (b - 26) + int_a
            else
              if b < 62
              then (b - 52) + int_0
              else if b = 62 then c62 else c63
        let char_to_sixbit c =
          if (int_A <= c) && (c <= int_Z)
          then Some (c - int_A)
          else
            if (int_a <= c) && (c <= int_z)
            then Some ((c - int_a) + 26)
            else
              if (int_0 <= c) && (c <= int_9)
              then Some ((c - int_0) + 52)
              else
                if c = c62
                then Some 62
                else if c = c63 then Some 63 else None
        let encode_buf ?(offset= 0)  ?len  (output : Buffer.t)
          (input : bytes) =
          let (input_offset, input_end, input_length) =
            let orig_len = Bytes.length input in
            let len = len |? (orig_len - offset) in
            let end_index = offset + len in
            if (len < 0) || (end_index > orig_len)
            then
              invalid_arg'
                "Base64.encode: the input range (offset:%d, len:%d) is out of bounds"
                offset len
            else (offset, end_index, len) in
          let output_buf =
            let estimated_chars = ((input_length / 3) * 4) + 4 in
            Buffer.create estimated_chars in
          let write i o len =
            let set value o =
              Buffer.add_uint8 output_buf (sixbit_to_char (value land 0x3f));
              o + 1 in
            let get i = Bytes.get_uint8 input i in
            let b1 = get i in
            let o = o |> (set (b1 lsr 2)) in
            match len with
            | `I -> o |> (set (b1 lsl 4))
            | `S n ->
                let b2 = get (i + 1) in
                let o = o |> (set ((b1 lsl 4) lor (b2 lsr 4))) in
                (match n with
                 | `I -> o |> (set (b2 lsl 2))
                 | `S `I ->
                     let b3 = get (i + 2) in
                     (o |> (set ((b2 lsl 2) lor (b3 lsr 6)))) |> (set b3)) in
          let rec go i o =
            match input_end - i with
            | 0 ->
                (match pad with
                 | Some pad ->
                     let pad_chars =
                       match o mod 4 with
                       | 0 -> 0
                       | 2 -> 2
                       | 3 -> 1
                       | _ -> failwith "impossible" in
                     (List.range 0 pad_chars) |>
                       (List.fold_left
                          (fun o ->
                             fun _ -> Buffer.add_char output_buf pad; o + 1)
                          o)
                 | None -> o)
            | 1 -> (`I |> (write i o)) |> (go (i + 1))
            | 2 -> ((`S `I) |> (write i o)) |> (go (i + 2))
            | _ -> ((`S (`S `I)) |> (write i o)) |> (go (i + 3)) in
          let total_bytes = go input_offset 0 in
          Buffer.add_buffer output output_buf; total_bytes
        let encode ?offset  ?len  input =
          let output = Buffer.create 0 in
          (encode_buf ?offset ?len output input) |> ignore;
          Buffer.contents output
        let decode_buf ?(offset= 0)  ?len  (output : Buffer.t)
          (input : string) =
          let input = Bytes.of_string input in
          let (input_offset, input_end, input_length) =
            let orig_len = Bytes.length input in
            let len = len |? (orig_len - offset) in
            let end_index = offset + len in
            if (len < 0) || (end_index > orig_len)
            then
              invalid_arg'
                "Base64.encode: the input range (offset:%d, len:%d) is out of bounds"
                offset len
            else
              if (pad <> None) && ((len mod 4) <> 0)
              then invalid_arg "Base64.decode: wrong padding"
              else (offset, end_index, len) in
          let output_buf =
            let estimated_bytes = ((input_length / 4) * 3) + 2 in
            Buffer.create estimated_bytes in
          let read stack o =
            let set o value =
              Buffer.add_uint8 output_buf (value land 0xff); o + 1 in
            match List.rev stack with
            | [] -> o
            | _::[] -> invalid_arg "Base64.decode: unterminated input"
            | s1::s2::ss ->
                let o = set o ((s1 lsl 2) lor (s2 lsr 4)) in
                (match ss with
                 | [] ->
                     if not ((s2 land 0xf) = 0)
                     then invalid_arg "Base64.decode: unterminated input"
                     else o
                 | s3::ss ->
                     let o = set o ((s2 lsl 4) lor (s3 lsr 2)) in
                     (match ss with
                      | [] ->
                          if not ((s3 land 0x3) = 0)
                          then
                            invalid_arg "Base64.decode: unterminated input"
                          else o
                      | s4::[] -> set o ((s3 lsl 6) lor s4)
                      | _ -> failwith "impossible")) in
          let rec go stack i o =
            if i = input_end
            then read stack o
            else
              (let c = Bytes.get_uint8 input i in
               match char_to_sixbit c with
               | None ->
                   (match char_of_int c with
                    | _ when ignore_unknown -> go stack (i + 1) o
                    | '\r' | '\n' when ignore_newline -> go stack (i + 1) o
                    | c ->
                        (match pad with
                         | Some pad when c = pad ->
                             let rest =
                               Bytes.sub_string input i (input_end - i) in
                             let valid =
                               match String.length rest with
                               | rest_len when
                                   (rest_len > 0) && (rest_len <= 2) ->
                                   iotafl rest_len
                                     (fun acc ->
                                        fun i -> acc && ((rest.[i]) = pad))
                                     true
                               | _ -> false in
                             if valid
                             then read stack o
                             else
                               invalid_arg'
                                 "Base64.decode: invalid char '%c' at index %d"
                                 pad i
                         | _ ->
                             invalid_arg'
                               "Base64.decode: invalid char '%c' at index %d"
                               c i))
               | Some s ->
                   let stack = s :: stack in
                   if (List.length stack) = 4
                   then let o = read stack o in go [] (i + 1) o
                   else go stack (i + 1) o) in
          let total_bytes = go [] input_offset 0 in
          Buffer.add_buffer output output_buf; total_bytes
        let decode ?offset  ?len  input =
          let output = Buffer.create 0 in
          (decode_buf ?offset ?len output input) |> ignore;
          Buffer.to_bytes output
      end 
    module Config_rfc4648 : Config =
      struct
        let c62 = '+'
        let c63 = '/'
        let pad = Some '='
        let ignore_newline = false
        let ignore_unknown = false
      end 
    include (Make)(Config_rfc4648)
    module Config_rfc4648_url : Config =
      struct
        let c62 = '-'
        let c63 = '_'
        let pad = None
        let ignore_newline = false
        let ignore_unknown = false
      end 
    module Url = (Make)(Config_rfc4648_url)
  end
