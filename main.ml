open Parsers
open Kernel
open Api

let _ =
  let ctsinp = Parser.input_from_file "cts.dk" in
  let env = Api.Env.init ctsinp in
  let norm = Env.reduction env in
  let envsig = Env.get_signature env in

  (* Doesn't print anything *)
  Signature.iter_symbols 
    (fun _ id _ ->
      Format.printf "identifier : %s\n" (Basic.string_of_ident id))
     envsig;

  let inp = Parser.input_from_file "testfile.dk" in
  let testdecl : Entry.entry = List.hd @@ Parser.parse inp in
  let ty = match testdecl with
  | Entry.Decl (_,_,_,_,t) -> t | _ -> failwith "unexpected" in

  (* Throws error (can't find cts.Term in signature) *)
  Format.printf "%a\n" Term.pp_term (norm ty) 

















  (* let rec gen : Term.term -> Term.term = function
    | Const (l, name) ->
      let open Basic in
      let newident = 
        let ident = string_of_ident (id name) in
        if ident = "box" then 
          mk_name (mk_mident "?") (mk_ident "?") 
        else name 
      in
      Const (l,newident)
    | App (f, a1, alist) ->
        App (gen f, gen a1, List.map gen alist)
    | Lam (l, id, ty, te) ->
        Lam(l, id, Option.map gen ty, gen te)
    | Pi (l, id, ty, te) ->
        Pi (l, id, gen ty, gen te)
    | x -> x
  in *)
  (* let leibniz : Entry.entry = match leibniz with
  | Entry.Decl (l,id,scp,st,t) -> Entry.Decl (l,id,scp,st,gen t)
  | _ -> failwith "not expected (1)"
  in *)