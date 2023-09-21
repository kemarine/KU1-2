let rec calculator x =
  let k = 0 in
  match x with
  | INT n -> n
  | X -> k
  | ADD (a, b) -> calculator a + calculator b
  | SUB (a, b) -> calculator a - calculator b
  | MUL (a, b) -> calculator a * calculator b
  | DIV (a, b) ->
  if calculator b = 0 then raise (Failure "Exception: Division_by_Zero")
  else calculator a / calculator b
  | SIGMA (m, n, e) ->
    let k = calculator m in
    (match m with
    | n -> calculator e
    | _ -> calculator e + calculator SIGMA (ADD(m, INT 1), n, e))
