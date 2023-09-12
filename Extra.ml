(* Append function *)
# let rec append l1 l2=
  match l1 with
  | n::[] -> n::l2
  | n::t -> n::append t l2
  | [] -> l2;;

(* Reverse *)
# let rec reverse l =
  match l with
  | [] -> []
  | hd::tl -> reverse tl @ [hd];;

(* Nth-element *)
# let rec nth l n =
  match l with
  | [] -> raise (Failure "list is too short")
  | hd::tl -> if n = 0 then hd else nth tl (n-1);;

(* Remove-first *)
# let rec remove_first a l =
  match l with
  | [] -> []
  | hd::tl -> if hd = a then tl else [hd] @ remove_first a tl;;

(* Insert (in sorted list) *)
# let rec insert a l =
  match l with
  | [] -> [a]
  | hd::tl -> if hd > a then a::l else hd::(insert a tl);;

(* Insertion sort *)
# let rec sort l =
  match l with
  | [] -> []
  | hd::tl -> insert hd (sort tl);;




