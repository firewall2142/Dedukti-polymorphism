open Api
module C = Conv
module T = Kernel.Term
module B = Kernel.Basic
module E = Parsers.Entry
(* 
module type I = sig
  val whitelist : B.ident list
  val env : Env.t
end

module type S = sig
  val transform : E.entry -> E.entry
  val get_rules : unit -> string
end
 *)

let whitelist = ref []

(** A map of terms with their polymorphics terms equivalent:
    Like `leibniz` to `leibniz_p cts.triangle ...`*)
let te_map : (T.term * T.term) list ref = ref []

let poly_app_term mid ident args =
  let f = T.mk_Const B.dloc (B.mk_name mid ident) in
  match args with
  | [] -> f
  | x :: tl -> T.mk_App f x tl

let rec preprocess te = let open T in match te with
| Const _ ->
  Option.value ~default:te
    (List.find_map 
      (fun (u,v) -> if T.term_eq te u then Some v else None) 
    !te_map)
| App (f, t1, ts) ->
    T.mk_App (preprocess f) (preprocess t1) 
      (List.map preprocess ts)
| Lam (l,id,ty_opt,te) ->
  let ty' = Option.bind ty_opt (fun x->Some (preprocess x)) in
  T.mk_Lam l id ty' (preprocess te)
| Pi (l,id,a,b) -> 
    T.mk_Pi l id (preprocess a) (preprocess b)
| Kind | Type _ | DB _ -> te

(** [build_entry env fmt entry]  Builds new entry from the given entry
    performing [preprocess] on the terms inside it. Also prints the
    corresponding rule to fmt *)
let build_entry env rule_fmt entry =
  let newid = fun id -> B.mk_ident @@ (B.string_of_ident id)^"_p" in
  let mid = Env.get_name env in
  let entry = match entry with
  | E.Def (l,id,sc,opq,_,te) ->
    begin
      if List.exists (B.ident_eq id) !whitelist then 
        begin
          let te = preprocess te in
          let te = Vars.add_vars @@ te in
          let (te',args) = Poly.gen_poly env false te in
          te_map := (T.mk_Const B.dloc (B.mk_name mid id), poly_app_term mid (newid id) args) :: !te_map;
          Format.fprintf rule_fmt "%a --> %a %a.\n"
            B.pp_name (B.mk_name mid id)
            B.pp_name (B.mk_name mid @@ newid id)
            Format.(pp_print_list ~pp_sep:pp_print_space T.pp_term)
            args;
          E.Def (l,newid id,sc,opq,None,te')
        end 
      else E.Def (l,id,sc,opq,None,preprocess te)
    end
  | E.Decl (l,id,sc,st,t) ->
    begin
      if List.exists (B.ident_eq id) !whitelist then 
        begin
          let t = preprocess t in
          let t = Vars.add_vars t in
          let (t',args) = Poly.gen_poly env true t in
          te_map := (T.mk_Const B.dloc (B.mk_name mid id), poly_app_term mid (newid id) args) :: !te_map;
          Format.fprintf rule_fmt "%a --> %a %a.\n"
            B.pp_name (B.mk_name mid id)
            B.pp_name (B.mk_name mid @@ newid id)
            Format.(pp_print_list ~pp_sep:pp_print_space T.pp_term)
            args;
          E.Decl (l,newid id,sc,st,t')
        end
      else E.Decl (l,id,sc,st,preprocess t)
    end
  | _ -> failwith "Entry neither Def or Decl"
  in
  entry