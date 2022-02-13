open Api
open Parsers
module C = Conv
module T = Kernel.Term
module B = Kernel.Basic

let curmid = ref (B.mk_mident "")

module P = Pp.Make (struct let get_name () = !curmid end)

let files = ["cts.dk"]

let test env =
  curmid := B.mk_mident "testfile";
  let entry = List.hd @@ Parser.(parse @@ input_from_file "testfile.dk") in
  match entry with
  | Entry.Def (l,id,sc,opq,_,te) ->
    begin
      let te = Vars.add_vars @@ te in
      let te' = Poly.gen_poly env false te in
      P.print_entry Format.std_formatter @@
        Entry.Def (l,id,sc,opq,None,te')
    end
  | Entry.Decl (l,id,sc,st,t) ->
    begin
      let t = Vars.add_vars t in
      let t' = Poly.gen_poly env true t in
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
