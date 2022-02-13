module T = Kernel.Term
module B = Kernel.Basic

type t = {
(*  env : Api.Env.t;  (** The current environement used for type checking *)*)
  cstrs : (T.term * T.term) list;
}

(* This is a reference because we have to use it in the Reduction Engine *)

(** [global_cstr] is a reference to the current type checking environment. *)
let global_cstr : t ref = ref {cstrs=[]}
let get = function
| None -> failwith "Env not set"
| Some env -> env

let is_uvar : T.term -> bool = function
| Const (_,name) ->
  let reg = Str.regexp "\\?_u[0-9]+" in
  Str.string_match reg B.(string_of_ident (id name)) 0
| _ -> false

let is_tvar : T.term -> bool = function
| Const (_,name) ->
  let reg = Str.regexp "\\?_t[0-9]+" in
  Str.string_match reg B.(string_of_ident (id name)) 0
| _ -> false

let tvar_of_uvar t = match t with
| T.Const (_, name) when is_uvar t -> 
    let id' = Str.(replace_first (regexp "u") "t") 
      B.(string_of_ident @@ id @@ name) in
    let name' = B.(mk_name (md name) (mk_ident id')) in
    T.mk_Const (B.dloc) name'
| _ -> raise @@ Invalid_argument "expected a constant"

let uvar_of_tvar t = match t with
| T.Const (_, name) when is_tvar t -> 
    let id' = Str.(replace_first (regexp "t") "u") 
      B.(string_of_ident @@ id @@ name) in
    let name' = B.(mk_name (md name) (mk_ident id')) in
    T.mk_Const (B.dloc) name'
| _ -> raise @@ Invalid_argument "expected a constant"

let uvar_of_id mid i =
  let id = B.mk_ident ("?_u"^(string_of_int i)) in
  let name = B.mk_name mid id in
  T.mk_Const (B.dloc) name

let tvar_of_id mid i =
  let id = B.mk_ident ("?_t"^(string_of_int i)) in
  let name = B.mk_name mid id in
  T.mk_Const (B.dloc) name

let is_var t = is_uvar t || is_tvar t

let md_of_var x =
  assert (is_var x); match x with
  | T.Const (_, name) -> B.md name
  | _ -> failwith "should not happen"

let ident_of_var t = match t with
| T.Const (_, name) when is_var t -> B.id @@ name
| _ -> raise @@ Invalid_argument "expected a Variable"

let id_of_var t =
  let s = B.(string_of_ident @@ ident_of_var t) in
  let r = Str.regexp "[0-9]+" in
  try let _ = (Str.search_forward r s 0) in
    int_of_string @@ Str.matched_string s
  with Not_found -> failwith "not found id"


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
    else if (is_var r) || (is_var l) then
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
