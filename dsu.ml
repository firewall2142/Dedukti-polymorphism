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

  module PH = Hashtbl.Make (E)

  type t = {
    par : E.t PH.t;
    rank : (int*int) PH.t
  }

  type elt = E.t

  let empty () = 
    {par = PH.create 100; rank = PH.create 100}

  let rec repr dsu x =
    match PH.find_opt dsu.par x with
    | None ->
        PH.(replace dsu.par x x; replace dsu.rank x (1,1); x)
    | Some y -> if not (E.equal x y) then repr dsu y else x

  let unite dsu x y =
    let open PH in
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


  let mem dsu = PH.mem dsu.rank

  let eles dsu = PH.to_seq_keys dsu.rank

  let all_repr dsu =
    Seq.filter (fun x -> E.equal x (repr dsu x)) @@ eles dsu

  let set_rank dsu x r =
    let xp = repr dsu x in
    if E.equal x xp then () else begin
      let rx = PH.find dsu.rank x in
      let rxp = PH.find dsu.rank xp in
      let rxp' = (fst rxp, (snd rxp)-(snd rx)) in
      PH.replace dsu.rank xp rxp';
      PH.replace dsu.rank x (r, snd rx);
      PH.replace dsu.par x x;
      unite dsu xp x;
      assert (E.equal (repr dsu x) x);
      assert (E.equal (repr dsu xp) x)
    end
end

