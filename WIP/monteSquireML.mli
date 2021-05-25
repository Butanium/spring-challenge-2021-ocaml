val isPossible : int -> int array -> action -> bool
(** [isPossible sun sizeArrays action] returns true if the action can be done this turn *)

val getSelfShadow : int -> float
(** [getSelfShadow t] returns +tree.size per ally tree hidden by t and
-tree.size per enemy tree strictly hidden by t*)

val getSelfShadowSize : int -> int -> float
(** [getSelfShadowSize p s] run getSelfShadow as if there where a tree of size s at the position p *)

val toString : action -> string
(** [toString a] returns the string representation of the action *)

val getNearTiles : int -> int -> int list
(** [getNearTiles t depth] returns all the tiles at the range depth of the t position *)

val getActionTrees : tree list -> action list
(** [getActionTrees trees] returns all the action that can be done by trees (doesn't care about
the tree being dormant or not and about the cost of the action) *)

val getPrice : int array -> action -> int
(**[getPrice sizeCount a] returns the amount of sun needed to perform the action a *)

val getTarget : action -> int
(** [getTarget a] returns the tree that need to be awaken for the action to be performed *)

val richBonus : int -> int
(** [richBonus t] returns the richness bonus that will provide this cell *)

val getScore : action -> float
(** [getScore a] returns 2 score : the first one is the valueability of the action comparing to other action
and the second is a secondary filter to choose between 2 action with the same primary score *)

val getDiff : tree -> tree -> int -> int -> int
(** [getDiff treeInit newTree turns day] returns the difference of incomes for the turns next turns if
treeInit is replaced per newTree*)

val getMCTSActions : int -> int option -> int -> mutableTree array -> int -> RndQ.t
(** [getMCTSActions player oldSeed sun treeArray] returns the possible action for player knowing
if the opponent seeded last turn and the treeArray of the current state *)

val mapInPlace : ('a  -> 'a) -> 'a array -> unit
(** [mapInPlace f arr] change each element a of arr into f a *)

val copyInPlace : 'a array -> 'a array -> unit
(** [copyInPlace model cont] copy each element of model in cont *)

val getWinner : int*int -> int*int -> int array array -> float
(** [getWinner playerSuns playerScores playerTrees] returns 1. if player 0 won, 0.5 if it's a draw and 0. if player 1 won *)
