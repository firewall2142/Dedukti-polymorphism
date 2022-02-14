module T = Kernel.Term
module B = Kernel.Basic
module Env = Api.Env

let var_cnt = ref 0
let var_map = ref []

(** To make a variable *)
let sorts = ["ball"; "box"; "diamond"; "star"; "sinf"; "triangle"; "I";]
let modid = B.mk_mident "var"

let reset () =
  var_cnt := 0;
  var_map := []

let uvar_of_id mid i =
  let id = B.mk_ident ("?_u"^(string_of_int i)) in
  let name = B.mk_name mid id in
  T.mk_Const (B.dloc) name

let tvar_of_id mid i =
  let id = B.mk_ident ("?_t"^(string_of_int i)) in
  let name = B.mk_name mid id in
  T.mk_Const (B.dloc) name

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

let is_var t = is_uvar t || is_tvar t

let is_dbvar = function
| T.DB (_,id,_) ->
    let s = B.string_of_ident id in
    Str.(string_match (regexp "?_[ut][0-9]+") s 0)
| _ -> false

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

let add_vars t =
  let rec aux t = let open T in match t with
  | Const (_, name) when List.mem B.(string_of_ident @@ id name) sorts ->
    begin
      incr var_cnt;
      let u = uvar_of_id modid !var_cnt in
      var_map := (u,t) :: !var_map;
      u
    end
  | App (f,a1,al) -> 
      T.mk_App (aux f) (aux a1) (List.map aux al)
  | Lam (l,id,topt,body) ->
    begin
      let t =
        match topt with
        | None -> (incr var_cnt; uvar_of_id modid !var_cnt)
        | Some x -> (aux x)
      in
      T.mk_Lam l id (Some t) (aux body)
    end
  | Pi (l,id,a,b) ->
      T.mk_Pi l id (aux a) (aux b)
  | Kind | Type _ | DB _ | Const _ -> t
  in
  aux t
