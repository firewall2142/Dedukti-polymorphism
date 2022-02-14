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

(** [preprocess te] substitutes any occurence of term [u] to term [v]
    in [te]. this is done of for all [(u,v)] in [te_map] *)
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

(** [build_entry env rule_fmt entry] Builds a list of entries from the given entry
    performing [preprocess] on the terms inside it. 
    There are two entries ( polymorphic and non-polymorphic) in the result if the
    identifier is whitelisted otherwise there is only one entry (non-polymorphic).
    Also prints the corresponding rule to rule_fmt. *)
let build_entry env rule_fmt entry =
  let newid = fun id -> B.mk_ident @@ (B.string_of_ident id)^"_p" in
  let mid = Env.get_name env in
  (* Format.eprintf "TEMAP :\n";
  List.iter (fun (u, v) -> Format.eprintf "%a --> %a" T.pp_term u T.pp_term v) !te_map;
  Format.eprintf "\n%!"; *)
  match entry with
  | E.Def (l,id,sc,opq,_,te) ->
    let te = preprocess te in
    let ty = C.Typing.infer (Env.get_signature env) [] te in
    let entries = [E.Def (l,id,sc,opq,Some ty,te)] in (* Initialize list with non polymorphic form of entry*)
    let entries = 
      begin
        if List.exists (B.ident_eq id) !whitelist then 
          begin
            let te = Vars.add_vars @@ te in
            let (te',args) = Poly.gen_poly env false te in
            te_map := (T.mk_Const B.dloc (B.mk_name mid id), poly_app_term mid (newid id) args) :: !te_map;
            Format.fprintf rule_fmt "[] %a --> %a %a.\n%!"
              B.pp_name (B.mk_name mid id)
              B.pp_name (B.mk_name mid @@ newid id)
              Format.(pp_print_list ~pp_sep:pp_print_space T.pp_term) args;
            let ty' = C.Typing.infer (Env.get_signature env) [] te' in
            (E.Def (l,newid id,sc,opq,Some ty',te')) :: entries
          end 
        else entries
      end
    in
    entries
  | E.Decl (l,id,sc,_,t) ->
    let st = Kernel.Signature.Definable T.Free in
    let t = preprocess t in
    let entries = [E.Decl (l,id,sc,st,t)] in (* Initialize list with non polymorphic form of entry*)
    begin
      if List.exists (B.ident_eq id) !whitelist then 
        begin
          let t = Vars.add_vars t in
          let (t',args) = Poly.gen_poly env true t in
          let id' = newid id in
          te_map := (T.mk_Const B.dloc (B.mk_name mid id), poly_app_term mid id' args) :: !te_map;
          Format.fprintf rule_fmt "[] %a --> %a %a.\n%!"
            B.pp_name (B.mk_name mid id)
            B.pp_name (B.mk_name mid id')
            Format.(pp_print_list ~pp_sep:pp_print_space T.pp_term) args;
          (E.Decl (l,id',sc,st,t')) :: entries
        end
      else entries
    end
  | _ -> failwith "Entry neither Def or Decl"