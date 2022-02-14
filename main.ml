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

let build env =
  curmid := B.mk_mident (
    let _ = Str.(search_forward (regexp "[^/]*.dk$") !poly_file 0) in
    let s = Str.matched_string !poly_file in
    Str.first_chars s ((String.length s) - 3)
  );
  (* let newid = fun id -> B.mk_ident @@ (B.string_of_ident id)^"_p" in *)
  let rule_fmt = Format.err_formatter in
  let entry_fmt = Format.std_formatter in
  let entries = Parser.(parse @@ input_from_file !poly_file) in
  List.iter (fun entry ->
    let entry = Procfile.build_entry env rule_fmt entry in
    P.print_entry entry_fmt entry) entries

let _ =
  Files.add_path "theory";
  let usage_msg = "Usage " ^ Sys.argv.(0) ^ "[OPTION]... [FILES]..." in
  let speclist = [
    ("-I", Arg.String Files.add_path, "Add to include path");
    ("-w", Arg.String (fun s -> 
        Procfile.(whitelist := (B.mk_ident s) :: !whitelist)), 
      "Whitelist identifier");
  ] in
  Arg.parse speclist (fun s -> poly_file := s) usage_msg;
  let hook_after env exn =
    match exn with
    | Some (env, lc, e) -> Env.fail_env_error env lc e
    | None -> build env
  in
  let hook = 
    Processor.{
      before = (fun _ -> Kernel.Confluence.initialize ());
      after = hook_after} 
  in
  Processor.handle_files [!poly_file] ~hook Processor.TypeChecker
