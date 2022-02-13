open Api
open Parsers
module C = Conv
module T = Kernel.Term
module B = Kernel.Basic

let curmid = ref (B.mk_mident "")

module P = Pp.Make (struct let get_name () = !curmid end)

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

let test env =
  curmid := B.mk_mident "testfile";
  let entry = List.hd @@ Parser.(parse @@ input_from_file "testfile.dk") in
  match entry with
  | Entry.Decl (l,id,sc,st,t) ->
    begin
      let t' = Poly.gen_poly env t in
      P.print_entry Format.std_formatter @@
        Entry.Decl (l,id,sc,st,t')
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
