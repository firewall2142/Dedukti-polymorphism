module T = Kernel.Term
module V = Vars

type t = {
(*  env : Api.Env.t;  (** The current environement used for type checking *)*)
  cstrs : (T.term * T.term) list;
}

(* This is a reference because we have to use it in the Reduction Engine *)

(** [global_cstr] is a reference to the current type checking environment. *)
let global_cstr : t ref = ref {cstrs=[]}

module MakeRE (Conv : Kernel.Reduction.ConvChecker) : Kernel.Reduction.S =
struct

  module rec R : Kernel.Reduction.S =
    Kernel.Reduction.Make (Conv) (Kernel.Matching.Make (R))

  module Rule = Kernel.Rule
  include R

  let add_cstr (l, r) =
    let cstrs = (l,r)::((!global_cstr).cstrs) in
    global_cstr := {cstrs=cstrs}

  let univ_conversion l r =
    let open T in
    if term_eq l r then 
      true
    else if (V.is_var r) || (V.is_var l) then
      (add_cstr (l, r); true)
    else false

  let rec are_convertible_lst sg : (T.term * T.term) list -> bool = function
  | [] -> true
  | (l, r) :: lst ->
    if T.term_eq l r then are_convertible_lst sg lst
    else
      let l', r' = (whnf sg l, whnf sg r) in
      if univ_conversion l' r' then are_convertible_lst sg lst
      else are_convertible_lst sg (R.conversion_step sg (l', r') lst)

  let are_convertible sg t1 t2 =
    try are_convertible_lst sg [(t1, t2)]
    with Kernel.Reduction.Not_convertible -> false

  let constraint_convertibility _cstr _ _ t1 t2 =
  if T.term_eq t1 t2 then true
  else failwith "Unexpected"
end

module rec RE : Kernel.Reduction.S = MakeRE (RE)

module Typing = Kernel.Typing.Make (RE)
