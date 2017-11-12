type symbol = string;;
type variable = string;;
type term = V of variable | Node of symbol * term list;;
type arity = int;;
type signature = (symbol * arity) list;;

(* Inert element a in set s *)
let insert a s = if List.mem a s then s else s @ a::[] ;; 


(**** Union ****)
let rec union s1 s2 = match s1 with 
                      [] -> s2
                      | x::xs -> insert x (union xs s2)
;;


(* check_sig : Given a signature consisting of symbols and their arities (>= 0)
it checks whether the signature is a valid signature (no repeated symbols,
arities are non-negative etc. *)

let check_sig (sign : signature) =
	(* Checking atleast one element with 0 arity *)
	List.exists (fun x -> if snd x = 0 then true else false) sign &&
	(* Checking no one element with negative arity *)
	not (List.exists (fun x -> if snd x < 0 then true else false) sign) &&
	(* Checking no repeated symbols *)
	let rec no_duplicates s old_s = match s with
		[] -> true
		|x::xs -> if List.mem (fst x) old_s then false
				  else no_duplicates xs ((fst x)::old_s) in
	no_duplicates sign []
;;


(* wfterm : Given a valid signature (checked using check_sig)
checks that a given preterm is well-formed according to the signature.
*)
let rec wfterm (t : term) (s : signature) = match t with
	V v -> true (* A variable is a well formed term *)
	| Node (symb, tlist) -> List.mem (symb, List.length tlist) s && (* this symbol has #children == its arity *)
							if List.length tlist <> 0 (* if this symbol has children *)
							(* Each of the children must be well formed *)
							then List.for_all (fun x -> if wfterm x s then true else false) tlist
							else true 
;;


(* ht : Given a well formed term returns its height. Leaves are at heiight 0 *)
let rec ht (t : term) = match t with
	V v -> 0 (* if this is a variable *)
	| Node (symb, tlist) -> if List.length tlist = 0 then 0 (* if this is a constant *)
							(* a symbol with arity > 0 *)
							else 1 + List.fold_left (fun a b -> max a (ht b)) 0 tlist
;;


(* size : Given a well formed term returns its size (number of nodes)*)
let rec size (t : term) = match t with
	V v -> 1 (* if this is a variable *)
	| Node (symb, tlist) -> if List.length tlist = 0 then 1 (* if this is a constant *)
							(* a symbol with arity > 0 *)
							else 1 + List.fold_left (fun a b -> a + (size b)) 0 tlist
;;


(* vars : Given a well formed term returns set of variables used in it*)
let rec vars (t: term) = match t with
	V v -> [v] (* if this term is variable then return the variable *)
	| Node (symb, tlist) -> if List.length tlist = 0 then [] (* if this is a constant term then return an empty list*)
							(* a symbol with arity > 0, return union of variables used int childrens *)
							else List.fold_left (fun a b -> union a (vars b)) [] tlist 
;;


(* List of (variable, term) pairs where each pair denotes which variable should be replaced which by term *)
type substitution = (variable * term) list;;


(* subst :  given a term t and a substitution s, applies the (Unique Homomorphic Extension of) s to t. *)
let rec subst (t:term) (sub:substitution) = match t with
	(* if this is variable and a substitution exists for it then do it *)
	V x ->  (try List.assoc x sub with
			 |Not_found -> t)
	| Node (symb, tlist) -> if List.length tlist = 0 then t
					  		else Node (symb, List.map ((fun s t -> subst t s) sub) tlist)
;;


(* substitution_composition : composition of two substitutions returns a subtitution *)
let rec substitution_composition sub1 sub2 =  sub2 @ (List.map ((fun sub lone_sub -> (fst lone_sub, subst (snd lone_sub) sub)) sub2) sub1);;


exception NOT_UNIFIABLE;;


(* mgu : given two terms t1 and t2, returns their most general unifier, 
if it exists and otherwise raises an exception NOT_UNIFIABLE.*)
let rec mgu t1 t2 = match (t1, t2) with
	(* if both are variables *)
	(V x, V y) -> if y = x then [] else [(x,V y)]
	(* if both are constants *)
	|(Node (symb1, []), Node (symb2, [])) -> if symb1 = symb2 then [] else raise NOT_UNIFIABLE
	(* 1 var and 1 constant *)
	|(V x, Node (symb2, [])) -> [(x, t2)]
	|(Node (symb1, []), V y) -> [(y, t1)]
	(* 1 variable and 1 non const symb *)
	|(V x, Node (symb, tlist))
	|(Node (symb, tlist), V x) -> (* If occurs check fails *)
								   if List.exists (fun tl -> List.mem x (vars tl)) tlist then raise NOT_UNIFIABLE
		   						   (* if occurs check passes *)
		   						   else [(x, Node(symb, tlist))]

    |(Node (symb1, tlist1), Node (symb2, tlist2)) -> 
    		if symb1 != symb2 && List.length tlist1 != List.length tlist2 then raise NOT_UNIFIABLE
 			else 
 			   List.fold_left2 (fun s t u -> substitution_composition s (mgu (subst t s) (subst u s))) [] tlist1 tlist2
	;;
