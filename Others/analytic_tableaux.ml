(* Data type for a proposition *)
type prop = Atom of string
	| T | F
	| Not of prop
	| And of prop * prop
	| Or of prop * prop
	| Imply of prop * prop ;;


(* Data type of a tableaux tree node *)
(* A node consisits of a proposition and the truth value we are trying to give it. *)
type node = prop * bool ;;

(* Data type of a tableaux tree *)
type tableaux = Closed  (* A leaf marking that no further analysis is required *)
	| Open (* A leaf marker here an extension is possible *)
	| Unary of node * tableaux (* rooted at a node that has one sub-tableau *) 
	| Binary of node * tableaux * tableaux;; (* Node that has two sub-tableau *)

exception IncorectTableau;;

