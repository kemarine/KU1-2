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
