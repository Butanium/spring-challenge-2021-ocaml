val isPossible : int -> int array -> action -> bool
(** [isPossible sun sizeArrays action] returns true if the action can be done this turn *)

val getSelfShadow : int -> float
(** [getSelfShadow t] returns +tree.size per ally tree hidden by t and
-tree.size per enemy tree strictly hidden by t*)

val getSelfShadowSize : int -> int -> float
(** [getSelfShadowSize p s] run getSelfShadow as if there where a tree of size s at the position p *)