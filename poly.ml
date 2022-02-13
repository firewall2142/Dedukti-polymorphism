module C = Conv
module T = Kernel.Term
module B = Kernel.Basic
module Env = Api.Env
module Topo = Topo.Make (struct type t=int let eq=(=) end)

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
    | Const _ when C.(is_var t) -> 
        let r = D.repr dsu t in
        if term_eq r t then r else aux r
    | App (f, t1, ts) ->
        T.mk_App (aux f) (aux t1) (List.map aux ts)
    | Lam (l,id,ty_opt,te) ->
        let ty' = Option.bind ty_opt (fun x->Some (aux x)) in
        T.mk_Lam l id ty' (aux te)
    | Pi (l,id,a,b) -> T.mk_Pi l id (aux a) (aux b)
    | _ -> t
  in
  aux te

(** Return DSU from [cstrs] *)
let gen_dsu cstrs =
  let dsu = D.empty () in
  List.iter (fun (x,y) -> 
    D.unite dsu x y;
    if not (C.is_var x) then D.set_rank dsu x 100 else ();
    if not (C.is_var y) then D.set_rank dsu y 100 else ())
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

(* Returns dependency list [(u, t)]*)
let gen_deplist dsu utlist =
  let deput (u,t) =
    List.filter_map (fun te -> 
        let te = D.repr dsu te in
        if C.is_var te then Some (C.id_of_var te)
        else None
      )
      ((deps u) @ (deps t))
  in
  List.map (fun (u,t) -> (C.id_of_var u, deput (u,t))) utlist

let quant_utlist te utlist =
  List.iter (fun (_,t) -> assert (not @@ C.is_var t)) utlist;
  let utlist = List.rev utlist in
  let ulist = List.map fst @@ utlist in
  (* Add (u1 : t1) -> ... -> te *)
  let te = List.fold_left 
    (fun acc (u,t) ->
      let ident = C.ident_of_var u in
      assert (not @@ C.is_var t);
      T.mk_Pi B.dloc ident t acc)
    te utlist
  in
  let te =
    let open T in
    let n = List.length ulist in
    let rec aux d t = match t with
    | Const _ when C.(is_var t) ->
        let _ = assert (C.is_uvar t) in
        T.mk_DB (B.dloc) (C.ident_of_var t) 
          (d + (List.find_ind t ~eq:term_eq ulist))
    | App (f, t1, ts) ->
        T.mk_App (aux d f) (aux d t1) (List.map (aux d) ts)
    | Lam (l,id,ty_opt,te) ->
        let ty' = Option.bind ty_opt (fun x->Some (aux d x)) in
        T.mk_Lam l id ty' (aux (d+1) te)
    | Pi (l,id,a,b) -> T.mk_Pi l id (aux d a) (aux (d+1) b)
    | Kind | Type _ | Const _ | DB _ -> t
    in
    aux (-n) te
  in
  Format.eprintf "\n\n%a\n\n%!" T.pp_term te;
  te

(* 
gen_poly env te
cstrs = generate constraints
dsu = generate dsu from cstrs
// replace elements in [te] with their parent
get a list of representative elements [ui]
  zip it with its type i.e. utlist = [(ui, ti)]
generate dependencies of utlist i.e. 
  utdep = [(i,[d1; d2; ...])] : (int * int list) list
topological sort the list w.r.t dependencies
create quantification from the list
*)
let gen_poly env ty =
  C.global_cstr := {cstrs=[]};
  let sg = Env.get_signature env in 
  let _ = C.Typing.infer sg [] ty in
  let cstrs = (!C.global_cstr).cstrs in
  List.iter (fun (l,r) -> Format.eprintf "%a ~~~ %a\n\n" T.pp_term l T.pp_term r) cstrs;
  let dsu = gen_dsu cstrs in
  let ty = repl_term dsu ty in (* Replace with parent in ty *)
  let utlist = (* ut list is list of [(?_u, ?_t)] where [?_t] is replaced with its parent *)
    List.filter_map (fun u -> 
        match T.term_eq (D.repr dsu u) u with
        | true -> Some (u, D.repr dsu (C.tvar_of_uvar u))
        | false -> None)
      (deps ty)
  in
  let mid = List.find_map (fun (u,t) ->
    if C.is_var u then Some (C.md_of_var u) else
    if C.is_var t then Some (C.md_of_var t) else
    None) utlist in
  let deplist = gen_deplist dsu utlist in
  let var_order = Topo.sort deplist in
  let utlist = List.map (fun i -> 
    let mid = Option.get mid in
    C.(uvar_of_id mid i, repl_term dsu @@ tvar_of_id mid i))
    var_order
  in
  quant_utlist ty utlist

(** Generate a polymorphic form of the term with quantification
    over the variables*)

