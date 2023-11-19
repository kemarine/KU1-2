(* Problem 1 : fibonacci number *)
let rec fib x =
  match x with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (x-1) + fib (x-2);;

(* Problem 2 : prime number *)
let rec pri n x =
  match n with
  | 0 -> true
  | 1 -> true
  | _ ->
  if x mod n = 0 then false
  else pri (n-1) x;;

let prime x = pri (x-1) x;;

(* Problem 3 : binarize *)

(* final commit version below *)

(* Problem 1 *)
let rec fib n =
  match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fib (n-1) + fib (n-2)

(* Problem 2 *)
let prime n =
  if n < 2 then false
  else
    let rec chk d =
      if n = d then true
      else
        if n mod d = 0 then false
        else
          chk (d + 1)
      in chk 2

(* Problem 3 *)
let binarize n =
  let rec bin n l =
    match n with
    | 0 ->
      if l == [] then [0] else l
    | _ -> bin (n/2) ((n mod 2)::l)
    in bin n [];;

(* Problem 4 *)
let sigma f a b =
  let rec prtsig f x b acc =
    if x <= b then prtsig f (x+1) b acc+(f x) else acc
  in prtsig f a b 0;;

(* Problem 5 *)
let iter: int * (int -> int) -> (int -> int) =fun (n,f) ->
  let rec loop fn num x =
    match num with
      | 0 -> x
      | _ -> loop fn (num - 1) (fn x)
  in loop f n;;

(* Problem 6 *)
let double: ('a -> 'a) -> 'a -> 'a
= fun f n -> f (f n);;

(* Problem 7 *)
let rec forall f l =
  match l with
    | [] -> raise (Failure "Empty list")
    | hd::[] -> f hd
    | hd::tl -> f hd && forall f tl

(* Problem 8 *)
let rec suml l =
  match l with
    | [] -> 0
    | hd::tl ->
      let rec sum l' =
        match l' with
          | [] -> 0
          | hd::tl -> hd + sum tl
        in sum hd + suml tl

(* Problem 9 *)
let rec max l =
  match l with
    | [] -> raise (Failure "Empty list")
    | hd::[] -> hd
    | hd::tl ->
      if hd > max tl then hd
      else max tl

let rec min l =
  match l with
    | [] -> raise (Failure "Empty list")
    | hd::[] -> hd
    | hd::tl ->
      if hd < min tl then hd
      else min tl

(* Problem 10 *)
let rec filter p l =
  match l with
    | [] -> []
    | hd::tl ->
      if p hd then hd::(filter p tl)
      else filter p tl

(* Problem 11 *)
let rec drop l n =
  match n, l with
  | 0, hd::tl -> l
  | _, hd::tl -> drop tl (n-1)
  | _, [] -> []

(* Problem 12 *)
let rec dropWhile f l =
  match l with
  | [] -> []
  | hd::tl ->
    if (f hd) then dropWhile f tl else hd::tl

(* Problem 13 *)
let zip x =
  let rec rzip y acc =
    match y with
      | ([], a) -> acc @ a
      | (a, []) -> acc @ a
      | (h1::t1, h2::t2) -> rzip (t1,t2) (acc @ [h1; h2])
  in rzip x []

(* Problem 14 *)
let unzip l =
  let rec runzip x (f,s) =
    match x with
      | [] -> (f,s)
      | (a, b)::t -> runzip t (f @ [a], s @ [b])
  in runzip l ([],[])

(* Problem 15 *)
let rec reduce f x y c =
  match x with
    | [] -> c
    | xh::xtl ->
      (match y with
        | [] -> c
        | yh::ytl -> reduce f xtl ytl (f xh yh c))

(* Problem 16 *)
type formula =
| True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp =
| Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec eval x =
  match x with
    | True -> true
    | False -> false
    | Not n -> not (eval n)
    | AndAlso(a, b) -> (eval a) && (eval b)
    | OrElse(a, b) -> (eval a) || (eval b)
    | Imply(a, b) ->
      if (eval a) && not (eval b) then false else true
    | Equal(a, b) ->
      let rec cal x =
        match x with
          | Num n -> n
          | Plus(a, b) -> (cal a) + (cal b)
          | Minus(a, b) -> (cal a) - (cal b)
        in (cal a) = (cal b)

(* Problem 17 *)
type nat = ZERO | SUCC of nat

let rec natadd a b =
  match a with
    | ZERO -> b
    | SUCC n -> natadd n (SUCC b)

let rec natmul a b =
  match a with
    | ZERO -> ZERO
    | SUCC ZERO -> b
    | SUCC n -> natadd b (natmul n b)

(* Problem 18 *)
type btree =
| Leaf of int
| Left of btree
| Right of btree
| LeftRight of btree * btree

let rec mirror n =
  match n with
    | Leaf n -> Leaf n
    | Left (x) -> Right (mirror x)
    | Right (x) -> Left (mirror x)
    | LeftRight (x, y) -> LeftRight(mirror y, mirror x);;

(* Problem 19 *)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec map f l =
  match l with
  | hd::tl -> f hd::map f tl
  | [] -> []

let rec diff (eq, v) =
  match eq with
    | Const n -> Const 0
    | Var x -> 
      if x = v then Const 1 else Const 0
    | Power (x, n) ->
      if x = v then Times [Power (x, n-1); Const n] else Const 0
    | Times al ->
      (let rec diff_Times i x acc =
        match i with
        | hd::tl -> [Times (acc @ (diff (hd, x)::tl))]@(diff_Times tl x (hd::acc))
        | [] -> []
      in Sum (diff_Times al v []))
    | Sum al ->
      let diff_v k = diff (k, v) 
      in Sum (map diff_v al)

(* Problem 20 *)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let rec cal e x=
  match e with
    | X -> x
    | INT n -> n
    | ADD (a, b) -> cal a x + cal b x
    | SUB (a, b) -> cal a x - cal b x
    | MUL (a, b) -> cal a x * cal b x
    | DIV (a, b) ->
      if b = (INT 0) then raise Division_by_zero
      else cal a x / cal b x
    | SIGMA (a, b, ex) ->
      let rec dosig a b e acc =
        if a <= b then
          dosig (a+1) b e (cal e a + acc)
        else
          acc
        in dosig (cal a x) (cal b x) ex 0

let calculator e = cal e 0;;
