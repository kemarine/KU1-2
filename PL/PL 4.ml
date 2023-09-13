(* insert item in back *)
# let rec insert a l =
  match l with
  | [] -> [a]
  | hd::tl -> hd::(insert a tl);;

(* efficient way to reverese *)
# let rec fast_rev l l' =
  match l with
  | [] -> l'
  | hd::tl -> fast_rev tl (hd::l');;

# let reverese l = fast_rev l [];;

