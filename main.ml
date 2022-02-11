open Api
open Parsers
module C = Conv
module T = Kernel.Term
module B = Kernel.Basic

let files = ["cts.dk"]

module D = Dsu.Make (struct
  open T
  type t = term
  let equal = term_eq
  (* consistent with equal *)
  let rec hash = fun t ->
  let comb a b = Hashtbl.hash (a, b) in
  match t with
  | Kind -> Hashtbl.hash 0
  | Type _ -> Hashtbl.hash 1
  | DB (_,_,n) -> Hashtbl.hash n
  | Const (_,nam) -> Hashtbl.hash nam
  | App (f, t1, ts) ->
      List.fold_left comb 0 @@
        List.map hash (f :: t1 :: ts)
  | Lam (_,_,_,b) -> hash b
  | Pi (_,_,a,b) -> comb (hash a) (hash b)
end)



let gen_term dsu te =
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


let test env =
  let entry = List.hd @@ Parser.(parse @@ input_from_file "testfile.dk") in
  C.global_cstr := {cstrs=[]};
  let dsu = D.empty () in
  match entry with
  | Entry.Decl (_,_,_,_,t) ->
    begin
      let sg = Env.get_signature env in
      C.Typing.check sg [] t (T.mk_Type B.dloc);
      let cstrs = (!C.global_cstr).cstrs in
      let unite (l,r) =
        Format.printf "%a ~~~ %a\n\n" T.pp_term l T.pp_term r;
        D.unite dsu l r
      in
      List.iter unite cstrs;

      (* Map containing [(v_repr, term)...] *)
      let _ (*te_map*) =
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
      in
      let new_te = gen_term dsu t in
      Format.printf "\nNew Term :\n%a\n" T.pp_term new_te
    end
  | _ -> failwith "unexpected entry"



let _ =
  let hook_after env exn =
    match exn with
    | Some (env, lc, e) -> Env.fail_env_error env lc e
    | None -> test env
  in
  let hook = 
    Processor.{
      before = (fun _ -> Kernel.Confluence.initialize ());
      after = hook_after} 
  in
  Processor.handle_files files ~hook Processor.TypeChecker
