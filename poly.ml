module C = Conv
module V = Vars
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
          res
        end
    | App (f, t1, ts) ->
        List.fold_left comb 0 @@
          List.map hash (f :: t1 :: ts)
    | Lam (_,_,_,b) -> hash b
    | Pi (_,_,a,b) -> comb (hash a) (hash b)
end)

let pp_list pp fmt l =
  let rec aux fmt = function
  | [] -> ()
  | x :: tl -> Format.fprintf fmt " %a;" pp x; aux fmt tl
  in
  Format.fprintf fmt "[%a]" aux l

(** Replace variables with their representative*)
let repl_term dsu te =
  let rec aux t = let open T in
    match t with
    | Const _ when V.(is_var t) -> 
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
  let get_rank t =
    if V.is_var t then 0 else
    if V.is_dbvar t then 200 else 100
  in
  let dsu = D.empty () in
  List.iter (fun (x,y) -> 
    D.unite dsu x y;
    if get_rank x > 0 then D.set_rank dsu x (get_rank x) else ();
    if get_rank y > 0 then D.set_rank dsu y (get_rank y) else ())
  cstrs;
  dsu

let rec deps te = let open T in
  match te with
  | Const _ when V.is_var te -> [te]
  | App (f, t1, ts) ->
      (deps f) @ (deps t1) @ 
        (List.flatten @@ List.map deps ts)
  | Lam (_,_,tyopt,b) -> 
      let l = Option.(value (map deps tyopt) ~default:[]) in
      l @ (deps b)
  | Pi (_,_,a,b) -> (deps a) @ (deps b)
  | _ -> []

(* Returns dependency list [(u, t)]*)
let gen_deplist dsu ulist =
  let depu u =
    let t = repl_term dsu (V.tvar_of_uvar u) in
    List.filter_map (fun te -> 
        let te = D.repr dsu te in
        if V.is_var te then Some (V.id_of_var te)
        else None
      )
      ((deps u) @ (deps t))
  in
  List.map (fun u -> (V.id_of_var u, depu u)) ulist


(** Generate a polymorphic form of the term with quantification
    over the variables*)
let quant_utlist te utlist use_pi =
  List.iter (fun (_,t) -> assert (not @@ V.is_var t)) utlist;
  let utlist = List.rev utlist in
  let ulist = List.map fst @@ utlist in
  (* Add (u1 : t1) -> ... -> te *)
  let te = List.fold_left 
    (fun acc (u,t) ->
      assert (not @@ V.is_var t);
      if use_pi then T.mk_Pi B.dloc V.(ident_of_var u) t acc
      else T.mk_Lam B.dloc V.(ident_of_var u) (Some t) acc)
    te utlist
  in
  let te =
    let open T in
    let n = List.length ulist in
    let rec aux d t = match t with
    | Const _ when V.(is_var t) ->
        let _ = assert (V.is_uvar t) in
        (* Format.eprintf "Finding : %a in [%a]\n" T.pp_term t (pp_list T.pp_term) ulist; *)
        let ind = List.find_ind t ~eq:term_eq ulist in
        T.mk_DB (B.dloc) (V.ident_of_var t) 
          (d + ind)
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
  (* Format.eprintf "\n\n%a\n\n%!" T.pp_term te; *)
  te

let add_lambdas utlist te =
  let rec aux = function
  | [] -> te
  | (u,t) :: tl -> 
    (T.mk_Lam B.dloc (V.ident_of_var u) (Some t) (aux tl))
  in
  aux utlist

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
let gen_poly env use_pi te =
  C.global_cstr := {cstrs=[]};
  let sg = Env.get_signature env in 
  let _ = ignore (C.Typing.infer sg [] te) in
  let cstrs = (!C.global_cstr).cstrs in
  (* List.iter (fun (l,r) -> Format.eprintf "%a ~~~ %a\n\n" T.pp_term l T.pp_term r) cstrs; *)
  let dsu = gen_dsu cstrs in
  let te = repl_term dsu te in (* Replace with parent in te *)
  (* ulist is list of representative ?_u variables *)
  (* Format.eprintf "te = %a\n" T.pp_term te;
  Format.eprintf "deps(te) = %a\n" (pp_list T.pp_term) (deps te); *)
  let ulist = List.filter (fun u -> T.term_eq (D.repr dsu u) u) (deps te) in
  (* Format.eprintf "ulist = %a\n" (pp_list T.pp_term) ulist; *)
  let deplist = gen_deplist dsu ulist in
  let var_order = Topo.sort deplist in
  let utlist = List.map (fun i -> 
    let mid = Vars.modid in
    V.(uvar_of_id mid i, repl_term dsu @@ tvar_of_id mid i))
    var_order
  in
  quant_utlist te utlist use_pi

