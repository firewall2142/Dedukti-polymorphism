module type S = sig
  type t
  type elt
  val empty : unit -> t
  val repr : t -> elt -> elt
  val unite : t -> elt -> elt -> unit
  val mem : t -> elt -> bool
  val eles : t -> elt Seq.t
  val all_repr : t -> elt Seq.t
  val set_rank : t -> elt -> int -> unit
end

module Make (E : Hashtbl.HashedType) = struct

  type t = {
    par : (E.t, E.t) Hashtbl.t;
    rank : (E.t, int * int ) Hashtbl.t
  }

  type elt = E.t

  let empty () = 
    {par = Hashtbl.create 100; rank = Hashtbl.create 100}

  let rec repr dsu x =
    match Hashtbl.find_opt dsu.par x with
    | None ->
        Hashtbl.(replace dsu.par x x; replace dsu.rank x (1,1); x)
    | Some y -> if not (E.equal x y) then repr dsu y else x

  let unite dsu x y =
    let open Hashtbl in
    if E.equal x y then () else begin
      let x = repr dsu x in
      let y = repr dsu y in
      let rankx = find dsu.rank x in
      let ranky = find dsu.rank y in
      let rank' = 
        (max (fst rankx) (fst ranky),(snd rankx)+(snd ranky)) 
      in
      if ranky > rankx then
          (replace dsu.par x y; replace dsu.rank y rank')
      else
          (replace dsu.par y x; replace dsu.rank x rank')
    end


  let mem dsu = Hashtbl.mem dsu.rank

  let eles dsu = Hashtbl.to_seq_keys dsu.rank

  let all_repr dsu =
    Seq.filter (fun x -> E.equal x (repr dsu x)) @@ eles dsu

  let set_rank dsu x r =
    let xp = repr dsu x in
    if E.equal x xp then () else begin
      let rx = Hashtbl.find dsu.rank x in
      let rxp = Hashtbl.find dsu.rank xp in
      let rxp' = (fst rxp, (snd rxp)-(snd rx)) in
      Hashtbl.replace dsu.rank xp rxp';
      Hashtbl.replace dsu.rank x (r, snd rx);
      Hashtbl.replace dsu.par x x;
      unite dsu xp x;
      assert (E.equal (repr dsu x) x);
      assert (E.equal (repr dsu xp) x)
    end
end

