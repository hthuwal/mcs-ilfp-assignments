type variable = string;;
type symbol = string;;
type term = V of variable | Node of symbol * term list;;
type arity = int;;
type signature = (symbol * arity) list;;


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
							else 1 + List.fold_left (fun a b -> max (ht b) a) 0 tlist
;;


(* size : Given a well formed term returns its size (number of nodes)*)
let rec size (t : term) = match t with
	V v -> 1 (* if this is a variable *)
	| Node (symb, tlist) -> if List.length tlist = 0 then 1 (* if this is a constant *)
							(* a symbol with arity > 0 *)
							else 1 + List.fold_left (fun a b -> (size b) + a) 0 tlist
;;
