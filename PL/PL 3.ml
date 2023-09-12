# Ocaml

(* defining new data types of natural number *)
# type mynat = Zero | Succ of mynat;;
# let rec myadd x y =
  match x with
  | Zero -> y
  | Succ n -> Succ (myadd n y);;
# let rec trans x =
  match x with
  | Zero -> 0
  | Succ n -> 1 + trans(n);;

(* creating binary tree *)
# type btree = Leaf | Br of int * bitree * bitree
# let t1 = Br(1, Br(2, Leaf, Leaf), Leaf);;
# let rec count_leaves t =
  match t with 
  | Leaf -> 1
  | Br(_, t1, t2) -> count_leaves t1 + count_leaves t2;;
# count_leaves t1;; (* - : int = 3 *)

(* abstract design of python *)
# type be = True | False;;
# type expr = Int of int | Str of string;;
# type stmt = 
    While of be * stmt 
  | For of expr * expr * expr * stmt 
  | Call of expr * expr;;

(* simple calculator *)
# type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr;;
# exception Division_by_zero;;
# exception TypeError;;
# let rec eval : expr -> int =
  fun expr -> match expr with
  | Int n -> n
  | Add (e1, e2) -> (eval e1) + (eval e2) 
  | Sub (e1, e2) -> (eval e1) - (eval e2) 
  | Mul (e1, e2) -> (eval e1) * (eval e2) 
  | Div (e1, e2) -> 
    if eval e2 = 0 then raise Division_by_zero
    else (eval e1) / (eval e2);;
# let error = Div (Int 5, Sub (Int 3, Int 3));;
# let rec typecheck : expr -> bool = fun expr -> true;; (* simplification *)
# let interpreter ;; (* need to be expanded *)
