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
        (arr'.(i-ofs) <- arr.(i)) |> ignore
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

