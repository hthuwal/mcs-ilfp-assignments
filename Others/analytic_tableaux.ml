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


(* analyseNode that analyses the node at the root of a given tableau t  and returns a list of tableaux
which will then be attached to end of each open path in the tableau t *)
(* analyseNode: tableau -> tableau list *)
let analyseNode node_t = match node_t with
	(T, true) -> [Open]
	|(T, false) -> [Closed]
	|(F, true) -> [Closed]
	|(F, false) -> [Open]
	|(Not p, truth) -> [Unary ((p, not truth), Open)]
	|(Or(p1,p2), truth) -> [Binary (()) 