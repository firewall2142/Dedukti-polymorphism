module C = Conv
module T = Kernel.Term
module B = Kernel.Basic
module Env = Api.Env

module D = Dsu.Make (struct
  open T
  type t = term
  let equal = term_eq
  (* consistent with equal *)
  let rec hash : T.term -> int = fun t ->
    let comb a b = Hashtbl.hash (a, b) in
    match t with
    | Kind -> Hashtbl.hash 0
    | Type _ -> Hashtbl.hash 1
    | DB (_,_,n) -> Hashtbl.hash n
    | Const (_,name) -> 
        begin
          (* let _ = B.(string_of_mident (md name)) in *)
          let id = B.(string_of_ident  (id name)) in
          let res = Hashtbl.hash id in
          (* Format.printf "Hash of %a = %d%!\n" T.pp_term t res; *)
          res
        end
    | App (f, t1, ts) ->
        List.fold_left comb 0 @@
          List.map hash (f :: t1 :: ts)
    | Lam (_,_,_,b) -> hash b
    | Pi (_,_,a,b) -> comb (hash a) (hash b)
end)


let te_map dsu cstrs =
        let tbl = Hashtbl.create 100 in
        let add2tbl (l, r) =
          let add v te = 
            let v = D.repr dsu v in
            match Hashtbl.find_opt tbl v with
            | None -> Hashtbl.replace tbl v te
            | Some te' -> assert (T.term_eq te' te)
          in
          if (C.is_var l) && (C.is_var r) then ()
          else if (C.is_var l) then add l r
          else if (C.is_var r) then add r l
          else assert (T.term_eq l r)
        in
        let _ = List.iter add2tbl cstrs in
        tbl

(** Replace variables with their representative*)
let repl_term dsu te =
  let rec aux t = let open T in
    match t with
    | Kind | Type _ | DB _ -> t
    | Const _ -> if C.(is_var t) then D.repr dsu t else t
    | App (f, t1, ts) ->
        T.mk_App (aux f) (aux t1) (List.map aux ts)
    | Lam (l,id,ty_opt,te) ->
        let ty' = Option.bind ty_opt (fun x->Some (aux x)) in
        T.mk_Lam l id ty' (aux te)
    | Pi (l,id,a,b) -> T.mk_Pi l id (aux a) (aux b)
  in
  aux te

(** Return DSU from [cstrs] *)
let gen_dsu cstrs =
  let dsu = D.empty () in
  List.iter (fun (x,y) -> 
    D.unite dsu x y;
    if not (C.is_var x) then 
      begin Format.printf "Setting rank of %a to 100\n" T.pp_term x;
      D.set_rank dsu x 100;
      assert (T.term_eq (D.repr dsu y) x) end
    else ();
    if not (C.is_var y) then 
      begin Format.printf "Setting rank of %a to 101\n" T.pp_term y;
      D.set_rank dsu y 101;
      end
    else ())
  cstrs;
  dsu

let rec deps te = let open T in
  match te with
  | Const _ when C.is_var te -> [te]
  | App (f, t1, ts) ->
      (deps f) @ (deps t1) @ 
        (List.flatten @@ List.map deps ts)
  | Lam (_,_,_,b) -> deps b
  | Pi (_,_,a,b) -> (deps a) @ (deps b)
  | _ -> []

(* 
gen_poly env te
cstrs = generate constraints
dsu = generate dsu from cstrs
// replace elements in [te] with their parent
get a list of representative elements [ui]
zip it with its type i.e. [(ui, ti)]
generate dependencies [(i,[d1; d2; ...])]
topological sort the list w.r.t dependencies
create quantification from the list
*)
let gen_poly env ty =
  C.global_cstr := {cstrs=[]};
  let sg = Env.get_signature env in 
  let _ = C.Typing.infer sg [] ty in
  let cstrs = (!C.global_cstr).cstrs in
  List.iter (fun (l,r) ->
      Format.printf "%a ~~~ %a\n\n" T.pp_term l T.pp_term r)
    cstrs;
  let dsu = gen_dsu cstrs in
  let repr = D.all_repr dsu in
  let utlist = Seq.fold_left
    (fun acc x -> 
      if C.is_uvar x then (x, D.repr dsu (C.tvar_of_uvar x))::acc
      else acc)
    [] repr in
  List.iter (fun (u, t) ->
      Format.printf "(%a : %a) ->\n" T.pp_term u T.pp_term t) 
    utlist;
  repl_term dsu ty

(* let add_quant dsu ty =
  let ty = repl_term dsu ty in
  let qv = ref [] in (* variables to quantify *)
  (* Ensure that the ordering is correct for terms *)
  let rec aux t : (T.term * T.term) list = 
    let open T in match t with
      | Const _ when C.(is_var t) ->
          qv := t :: (!qv)
      | App (f, t1, ts) ->
          List.iter aux (f::t1::ts)
      | Lam (_,_,ty_opt,te) ->
          (Option.iter aux ty_opt; aux te)
      | Pi (_,_,a,b) -> (aux a; aux b)
      | _ -> ()
  in
  aux ty *)

(** Generate a polymorphic form of the term with quantification
    over the variables*)

