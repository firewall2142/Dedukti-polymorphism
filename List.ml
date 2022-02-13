include Stdlib.List

(** [find_ind x ~eq l] the index of [x] in [l] *)
let find_ind x ?eq l =
  let eq = Option.value eq ~default:(=) in
  let rec aux n = function
  | [] -> raise Not_found
  | x' :: _ when (eq x' x) -> n
  | _ :: tl -> aux (n+1) tl
  in aux 0 l