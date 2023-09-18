(* Implemantation of simple language below
P → E
E → n
  | x
  | E + E
  | E - E
  | iszero E
  | if E then E else E
  | let x = E in E
  | read
*)

(* syntax definition *)
type program = exp
and exp =
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | READ
  | ISZERO of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
and var = string

(* Values *)
type value = Int of int | Bool of bool

(* Environments *)
type env = (var * value) list
let empty_env = []
let extend_env = (x, v) e = (x, v)::e
let rec apply_env e x =
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " not found"))
  | (y, v)::tl -> if x = y then v else apply_env tl x

(* Evaluation rules *)
let rec eval : exp -> env -> value
= fun exp env ->
  match exp with
  | CONST n -> Int n
  | VAR x -> apply_env env x
  | ADD (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> raise (Failure "Type Error: non-numerical values"))
  | SUB (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> raise (Failure "Type Error: non-numerical values"))
  | READ -> Int (read_int())
  | ISZERO e ->
    (match eval e env with
    | Int 0 -> Bool true
    | Int n -> Bool false
    | _ -> raise (Failure "Type Error: non-numerical values"))
  | IF (e1, e2, e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise (Failure "Type Error: condition must be Bool type")
  | LET (x, e1, e2) ->
    let v1 = eval e1 env in
      eval e2 (extend_env (x,v1) env)

(* Interpreter *)
let run : program -> value
= fun pgm -> eval pgm empty_env


