open Api
open Parsers
module C = Conv
module T = Kernel.Term
module B = Kernel.Basic

let curmid = ref (B.mk_mident "")

module P = Pp.Make (struct let get_name () = !curmid end)

(** Files to setup [Env.t] *)

(** Polymorphic definitions are generated from these files *)
let poly_file = ref "tests/testfile.dk"
let rule_file = ref "/tmp/rules.dk"
let cts_file = "theory/cts.dk"

let run rule_fmt entry_fmt env =
  curmid := B.mk_mident (
    let _ = Str.(search_forward (regexp "[^/]*.dk$") !poly_file 0) in
    let s = Str.matched_string !poly_file in
    Str.first_chars s ((String.length s) - 3)
  );
  let entries = Parser.(parse @@ input_from_file !poly_file) in
  let change_staticity en = match en with
  | Entry.Decl (l,id,sc,_,te) -> Entry.Decl (l,id,sc,Kernel.Signature.Definable T.Free,te)
  | _ -> en
  in
  List.iter 
    (fun entry ->
      let entry = change_staticity entry in
      let enlist = Procfile.build_entry env rule_fmt entry in
      List.iter (fun en ->
          P.print_entry entry_fmt en;
          match en with
          | Entry.Def (l,id,sc,opq,Some ty,te) -> 
            (try Env.define env l id sc opq te (Some ty)
            with e -> 
              Format.eprintf "Error while defining %a" P.print_entry en;
              raise e)
          | Entry.Decl (l,id',sc,st,t') -> Env.declare env l id' sc st t'
          | _ -> failwith "shouldn't happen") 
        enlist)
    entries

let _ =
  (* Add theory to file paths *)
  Files.add_path "theory";

  let usage_msg = "Usage " ^ Sys.argv.(0) ^ "[OPTION]... [FILES]..." in
  let speclist = [
    ("-I", Arg.String Files.add_path, "Add to include path");
    ("-w", Arg.String (fun s -> 
        Procfile.(whitelist := (B.mk_ident s) :: !whitelist)), 
      "Whitelist identifier");
    ("-m", Arg.Set_string rule_file, "Set meta file")
  ] in
  Arg.parse speclist (fun s -> poly_file := s) usage_msg;
  let env = Env.init (Parser.input_from_file !poly_file) in
  let rule_oc = open_out_gen [Open_append; Open_text] 666 !rule_file in
  try
    let rule_fmt = Format.formatter_of_out_channel rule_oc in
    let entry_fmt = Format.std_formatter in
    run rule_fmt entry_fmt env;
    close_out rule_oc
  with e -> close_out_noerr rule_oc; raise e
