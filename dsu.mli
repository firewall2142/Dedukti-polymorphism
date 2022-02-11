module type S = sig
  (** A DSU *)
  type t

  (** Type for element *)
  type elt

  (** Creates an empty DSU *)
  val empty : unit -> t

  (** Returns the representative for the element *)
  val repr : t -> elt -> elt

  (** Joins two elements *)
  val unite : t -> elt -> elt -> unit

  (** Check for membership. [x] is a member if
      [repr] or [unite] has been called with [x] *)
  val mem : t -> elt -> bool

  (** Seq of all elements *)
  val eles : t -> elt Seq.t

  (** Seq of all representative elements *)
  val all_repr : t -> elt Seq.t

  (** Sets rank of element (by default rank is 0), representative is
      the one with the highest rank *)
  val set_rank : t -> elt -> int -> unit
end


module Make (E : Hashtbl.HashedType) : S with type elt=E.t
