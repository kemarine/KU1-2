(* Do not open any module *)

(***********************)
(*  Library functions  *)
(***********************)

let rec fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun f accu lst ->
  match lst with
  | [] -> accu
  | hd::tl -> fold_left f (f accu hd) tl

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f lst ->
  match lst with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

(***********************)
(******  Syntax  *******)
(***********************)

type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

(***********************)
(*****  Problem 1  *****)
(***********************)

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of var * var * exp *
                     var * var * exp * env
and env = (var * value) list

exception UndefinedSemantics

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ fold_left (fun s x -> s ^ ", " ^ x) "" (map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl


let rec eval : exp -> env -> value
=fun exp env ->
  match exp with
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
  | CONST n -> Int n
  | VAR v -> lookup_env v env
  | ADD (a, b) ->
    (let an = eval a env in
    let bn = eval b env in
    match an, bn with  
    | Int an, Int bn -> Int (an + bn)
    | _ -> raise UndefinedSemantics)
  | SUB (a, b) ->
    (let an = eval a env in
    let bn = eval b env in
    match an, bn with
    | Int an, Int bn -> Int (an - bn)
    | _ -> raise UndefinedSemantics)
  | MUL (a, b) ->
    (let an = eval a env in
    let bn = eval b env in
    match an, bn with
    | Int an, Int bn -> Int (an * bn)
    | _ -> raise UndefinedSemantics)
  | DIV (a, b) ->
    (let an = eval a env in
    let bn = eval b env in
    if bn = Int 0 then raise UndefinedSemantics (* ERROR NAME FIX *)
    else
      match an, bn with
      | Int an, Int bn -> Int (an / bn)
      | _ -> raise UndefinedSemantics)
  | EQUAL (a, b) ->
    (let an = eval a env in
    let bn = eval b env in
    match an, bn with
    | Int an, Int bn -> Bool (an = bn)
    | Bool an, Bool bn -> Bool (an = bn)
    | _ -> raise UndefinedSemantics)
  | LESS (a, b) ->
    (let an = eval a env in
    let bn = eval b env in
    match an, bn with
    | Int an, Int bn -> Bool (an < bn)
    | _ -> raise UndefinedSemantics)
  | NOT e ->
    (match eval e env with
    | Bool x -> Bool (not x)
    | _ -> raise UndefinedSemantics)
  | NIL -> List []
  | CONS (e1, e2) ->
    (let e1 = eval e1 env in
    let e2 = eval e2 env in
    match e1, e2 with
    | e1, List e2 -> List (e1::e2) (* del e1? or save it for type comp? *)
    | _ -> raise UndefinedSemantics) (* add type comparison *)
  | APPEND (e1, e2) ->
    (let e1 = eval e1 env in
    let e2 = eval e2 env in
    match e1, e2 with
    | List e1, List e2 -> List (e1 @ e2) (* add type comparison *)
    | _ -> raise UndefinedSemantics)
  | HEAD e ->
    (let e = eval e env in
      match e with
    | List (hd::tl) -> hd
    | _ -> raise UndefinedSemantics)
  | TAIL e -> 
    (let e = eval e env in
    match e with
    | List (hd::tl) -> List tl
    | _ -> raise UndefinedSemantics)
  | ISNIL e ->
    (match eval e env with
    | List [] -> Bool true
    | _ -> Bool false)
  | IF (e1, e2, e3) ->
    (let e1 = eval e1 env in
    match e1 with
    | Bool x -> if x then eval e2 env else eval e3 env 
    | _ -> raise UndefinedSemantics)
  | LET (x, e1, e2) -> 
    let e1 = eval e1 env in let e2 = eval e2 (extend_env (x, e1) env) in e2
  | LETREC (f, x, e1, e2) ->
    let e = RecProcedure (f, x, e1, env) in eval e2 (extend_env (f, e) env)
  | PROC (x, e) -> Procedure (x, e, env)
  | CALL (e1, e2) -> 
    (let e1 = eval e1 env in
    let v = eval e2 env in
    match e1 with
    | Procedure (x, e, env1) -> eval e (extend_env (x, v) env1)
    | RecProcedure (f, x, e, env1) -> 
      eval e (extend_env (x, v) (extend_env (f, e1) env1))
    | MRecProcedure (f, x, ef, g, y, eg, env2) ->
      eval ef (extend_env (g, MRecProcedure(g, y, eg, f, x, ef, env2)) (extend_env (f, e1) (extend_env (x, v) env2)))
    | _ -> raise UndefinedSemantics)
  | LETMREC ((f, x, e1), (g, y, e2), e3) ->
    let env1 = extend_env (f, MRecProcedure (f, x, e1, g, y, e2, env)) env in
    let env2 = extend_env (g, MRecProcedure (g, y, e2, f, x, e1, env)) env1 in
    eval e3 env2
  | SEQ (e1, e2) ->
    let _ = eval e1 env in let e2 = eval e2 env in e2
  
let runml : program -> value
=fun pgm -> eval pgm empty_env;;

(***********************)
(*****  Problem 2  *****)
(***********************)

type typ = 
    TyUnit 
  | TyInt 
  | TyBool 
  | TyFun of typ * typ 
  | TyList of typ
  | TyVar of tyvar
and tyvar = string

exception TypeError

(* You can invoke "fresh_tyvar()" to generate a fresh type variable *)
let tyvar_num = ref 0;;
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)));;

let eq_exist = ref false;;
let rec lookup_tenv x e = 
  match e with
  | [] -> fresh_tyvar()
  | (y,v)::tl -> if x = y then v else lookup_tenv x tl;;

let rec gen_eqns tenv e typ =
  match e with
  | UNIT -> [(typ, TyUnit)]
  | TRUE -> [(typ, TyBool)]
  | FALSE -> [(typ, TyBool)]
  | CONST n -> [(typ, TyInt)]
  | VAR v -> [(typ, lookup_tenv v tenv)]
  | ADD (e1, e2)
  | SUB (e1, e2) 
  | MUL (e1, e2)
  | DIV (e1, e2) -> (typ, TyInt)::(gen_eqns tenv e1 TyInt @ gen_eqns tenv e2 TyInt)
  | EQUAL (e1, e2) -> 
    let nt1 = fresh_tyvar() in
    let nt2 = fresh_tyvar() in
    (eq_exist := true; (typ, TyBool)::(nt1, nt2)::(gen_eqns tenv e1 nt1 @ gen_eqns tenv e2 nt2))
  | LESS (e1, e2) -> (typ, TyBool)::(gen_eqns tenv e1 TyInt @ gen_eqns tenv e2 TyInt)
  | NOT e -> (typ, TyBool)::(gen_eqns tenv e TyBool)
  | NIL -> [(typ, TyList (fresh_tyvar()))]
  | CONS (e1, e2) ->
    let nt = fresh_tyvar() in
    (typ, TyList nt)::(gen_eqns tenv e1 nt @ gen_eqns tenv e2 (TyList nt))
  | APPEND (e1, e2) ->
    let nt = fresh_tyvar() in
    (typ, TyList nt)::(gen_eqns tenv e1 (TyList nt) @ gen_eqns tenv e2 (TyList nt))
  | HEAD e ->
    let nt = fresh_tyvar() in
    (typ, nt)::(gen_eqns tenv e (TyList nt))
  | TAIL e ->
    let nt = fresh_tyvar() in
    (typ, TyList nt)::(gen_eqns tenv e (TyList nt))
  | ISNIL e ->
    let nt = fresh_tyvar() in
    (typ, TyBool)::(gen_eqns tenv e (TyList nt))
  | IF (e1, e2, e3) ->
    let nt = fresh_tyvar() in
    (typ, nt)::(gen_eqns tenv e1 TyBool @ gen_eqns tenv e2 nt @ gen_eqns tenv e3 nt)
  | LET (x, e1, e2) -> 
    let nt1 = fresh_tyvar() in
    let nt2 = fresh_tyvar() in
    (typ, nt2)::(gen_eqns tenv e1 nt1 @ gen_eqns (extend_env (x, nt1) tenv) e2 nt2)
  | LETREC (f, x, e1, e2) ->
    let tx = fresh_tyvar() in
    let nt1 = fresh_tyvar() in
    let nt2 = fresh_tyvar() in
    (typ, nt2)::(gen_eqns (extend_env (f, TyFun(tx, nt1)) (extend_env (x, tx) tenv)) e1 nt1 @ gen_eqns (extend_env (f, TyFun(tx, nt1)) tenv) e2 nt2)
  | LETMREC ((f, x, e1), (g, y, e2), e3) -> 
    let nt1 = fresh_tyvar() in
    let nt2 = fresh_tyvar() in
    let nt3 = fresh_tyvar() in
    let tx = fresh_tyvar() in
    let ty = fresh_tyvar() in
    let fgtenv = extend_env (g, TyFun(ty, nt2)) (extend_env (f, TyFun(tx, nt1)) tenv) in
    (typ, nt3)::(gen_eqns (extend_env (x, tx) fgtenv) e1 nt1 @ gen_eqns (extend_env (y, ty) fgtenv) e2 nt2 @ gen_eqns fgtenv e3 nt3)
  | PROC (x, e) ->
    let nt1 = fresh_tyvar() in
    let nt2 = fresh_tyvar() in
    (typ, TyFun(nt1, nt2))::(gen_eqns (extend_env (x, nt1) tenv) e nt2)
  | CALL (e1, e2) ->
    let nt1 = fresh_tyvar() in
    let nt2 = fresh_tyvar() in
    (typ, nt2)::(gen_eqns tenv e1 (TyFun(nt1, nt2)) @ gen_eqns tenv e2 nt1)
  | PRINT e -> 
    let nt = fresh_tyvar() in
    (typ, TyUnit)::(gen_eqns tenv e nt)
  | SEQ (e1, e2) ->
    let nt1 = fresh_tyvar() in
    let nt2 = fresh_tyvar() in
    (typ, nt2)::(gen_eqns tenv e1 nt1 @ gen_eqns tenv e2 nt2);;

let rec subst t s=
  match t with
  | TyUnit -> TyUnit
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (x, y) -> TyFun (subst x s, subst y s)
  | TyList t -> TyList (subst t s)
  | TyVar n ->
    let rec lookup_subs x subs =
      match subs with
      | (t1, t2)::t -> if x = t1 then t2 else lookup_subs x t
      | [] -> x
    in lookup_subs t s;;
  
let rec occur_chk t1 t2 =
  if t1 = t2 then true
  else
  match t2 with
  | TyFun (x,y) -> occur_chk t1 x || occur_chk t1 y
  | TyList a -> occur_chk t1 a
  | _ -> false;;

let rec propagate t1 t2 s =
  let rec propa t1 t2 x =
    match x with
    | TyVar _ -> if x = t1 then t2 else x
    | TyFun (y, z) -> TyFun (propa t1 t2 y, propa t1 t2 z)
    | TyList n -> TyList (propa t1 t2 n)
    | _ -> x
  in
  match s with
  | (x, y)::t -> (x, propa t1 t2 y)::(propagate t1 t2 t)
  | [] -> s;;

let rec unify t1 t2 s =
  if t1 = t2 then s
  else 
    match t1, t2 with
  | n, TyVar _ -> 
    (match n with 
    | TyVar _ -> (t1, t2)::(propagate t1 t2 s) 
    | _ -> unify t2 t1 s) 
  | TyList a, TyList b ->
    (match a, b with
    | TyVar _, _ 
    | _, TyVar _ -> unify a b s
    | _ -> raise TypeError)
  | TyVar n, _ -> if occur_chk t1 t2 then raise TypeError else (t1, t2)::(propagate t1 t2 s)
  | TyFun (t1, t2), TyFun (t3, t4) -> 
    let s2 = unify t1 t3 (propagate t1 t2 s) in 
    let s3 = unify t2 t4 (propagate t1 t2 s2) in
    (propagate t1 t2 s3)
  | _ -> raise TypeError;;

let rec unifyall eqns s =
  match eqns with
  | [] -> s
  | (t1, t2)::t ->
    let s2 = unify (subst t1 s) (subst t2 s) s in
    unifyall t s2;;

let rec eq_tyvar typ =
  match typ with
  | TyVar _ -> TyInt
  | TyFun (a, b) -> TyFun (eq_tyvar a, eq_tyvar b)
  | TyList a -> TyList (eq_tyvar a)
  | _ -> typ

let typecheck : program -> typ 
=fun exp -> 
  let t = fresh_tyvar() in
  let s = unifyall (gen_eqns [] exp t) [] in
  let res = subst t s in
if !eq_exist then (eq_exist := false; eq_tyvar (res)) else res;;
