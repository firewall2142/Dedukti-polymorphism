include Stdlib.List

(** [find_ind x ~eq l] the index of [x] in [l] *)
let find_ind x ?eq l =
  let eq = Option.value eq ~default:(=) in
  let rec aux n = function
  | [] -> assert false
  | x' :: _ when (eq x' x) -> n
  | _ :: tl -> aux (n+1) tl
  in 
  aux 0 l

(** [range a b] generates [a; a+1; ... ; b-1] *)
let range a b =
  let rec aux acc n =
    if n >= b then [] else
    aux (n :: acc) (n+1)
  in
  Stdlib.List.rev @@ aux [] a