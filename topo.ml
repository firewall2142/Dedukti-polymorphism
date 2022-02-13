module type BT = sig
  type t
  val eq : t -> t -> bool
end

module type S = sig
  type t
  val sort : (t * t List.t) List.t -> t List.t
end

(** Thanks https://rosettacode.org/wiki/Topological_sort#OCaml !*)

module Make (E : BT) : (S with type t=E.t) = struct 
  type t = E.t
  let clean depl =
    (* remove self dependencies *)
    let cl = fun (x, depx) -> 
      (x, List.filter (fun d -> not (E.eq d x)) depx)
    in
    List.map cl depl

  let rev_unique =
    List.fold_left (fun acc x -> if List.exists (E.eq x) acc then acc else x::acc) []

  let all_items depl =  (* list items, each being unique *)
    rev_unique (List.flatten(List.map (fun (lib, deps) -> lib::deps) depl))

  let get_deps x depl =
    try List.assoc x depl
  with Not_found -> failwith "Can't find deps"

  let sort = fun dep_list ->
    let dep_list = clean dep_list in
    let rec aux acc later todo progress =
      match todo, later with
      | [], [] -> (List.rev acc)
      | [], _ ->
          if progress
          then aux acc [] later false
          else invalid_arg "un-orderable data"
      | x::xs, _ ->
          let deps = get_deps x dep_list in
          let ok = List.for_all (fun dep -> List.exists (E.eq dep) acc) deps in
          if ok
          then aux (x::acc) later xs true
          else aux acc (x::later) xs progress
    in
    let items = all_items dep_list in
    let starts, todo = List.partition (fun x -> get_deps x dep_list = []) items in
    aux starts [] todo false
end