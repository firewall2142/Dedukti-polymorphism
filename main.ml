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

