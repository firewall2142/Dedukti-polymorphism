module type BT = sig type t val eq : t -> t -> bool end
module type S = sig type t val sort : (t * t list) list -> t list end
module Make :
  functor (E : BT) ->
    sig type t = E.t val sort : (t * t list) list -> t list end
