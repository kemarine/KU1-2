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
