(* insert item in back *)
let rec insert a l =
  match l with
    | [] -> [a]
    | hd::tl -> hd::(insert a tl);;

(* efficient way to reverse *)
let rec fast_rev l l' =
  match l with
    | [] -> l'
    | hd::tl -> fast_rev tl (hd::l');;

let reverse l = fast_rev l [];;

(* remove all objects *)
let rec remove_all a l =
  match l with
    | [] -> []
    | hd::tl ->
      if hd = a then (remove_all a tl)
      else hd::(remove_all a tl);;

(* filter *)
let rec filter f l =
  match l with
    | [] -> []
    | hd::tl ->
      if f hd then hd::(filter f tl)
      else filter f tl;;

(* apply parameters *)
let even l = filter (fun x -> x mod 2 = 0) l;;

let greater_than_five l = filter (fun x -> x > 5) l;;

(* fold-right *)
let rec fold_right f l a =
  match l with
    | [] -> a
    | hd::tl -> f hd (fold_right f tl a);;

(* apply parameters *)
let sum lst = fold_right (fun x y -> x + y) lst 0;;
let sum lst = List.fold_right (+) lst 0;;

let prod lst = fold_right (fun x y -> x * y) lst 1;;
let prod lst = List.fold_right ( *) lst 1;;

(* fold_left *)
(* pattern analysis *)
let rec sum a l =
  match l with
    | [] -> a
    | hd::tl -> sum (hd + a) tl;;

let rec prod a l =
  match l with
    | [] -> a
    | hd::tl -> prod (hd * a) tl;;

(* generalize *)
let rec fold_left f a l =
  match l with
    | [] -> a
    | hd::tl -> fold_left (f a hd) tl;;

(* length in fold_right *)
let length l = fold_right (fun _ y -> y + 1) l 0;;

(* reverse in fold_right *)
let reverse l = fold_right (fun x y -> y @ [x]) l [];;

(* is_all_pos in fold_right *)
let is_all_pos l = fold_right (fun x y -> x > 0 && y) l true;;

let map f l = fold_right (fun x y -> (f x)::y) l [];;

let filter f l  = fold_right (fun x y -> if f x then x::y else y) l [];;
