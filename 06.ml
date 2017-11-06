type symbol = string;;
type variable = string;;
type predicate = string;;
type term = V of variable | Node of symbol * term list;; (* A term is either a variable, a constant (0 ary symbol), or a k-ary function symbol with k subterms. *)
type atomic_formula = Atom of predicate * term list;; (* An atomic formula is a k-ary predicate symbol followed by k terms. making it a node for unification*)
type fact = atomic_formula;;(* A fact has a head but no body *)
type rule = atomic_formula * atomic_formula list;; (* A rule has a head and a body.The head is a single atomic formula.A body is a sequence of atomic formulas. *)
type clause = Fact of fact | Rule of rule;; (*  A clause can either be a fact or a rule. *)
type program = clause list;; (* A program is a set (list) of clauses. *)
type goal = atomic_formula list;;(* A goal is a set (list) of atomic formulas. *)


(* List of (variable, term) pairs where each pair denotes which variable should be replaced which by term *)
type substitution = (variable * term) list;;



(* Inert element a in set s *)
let insert a s = if List.mem a s then s else s @ a::[] ;; 


(**** Union ****)
let rec union s1 s2 = match s1 with 
                      [] -> s2
                      | x::xs -> insert x (union xs s2)
;;

(* vars : Given a well formed term returns set of variables used in it*)
let rec vars (t:term) = match t with
	V v -> [v] (* if this term is variable then return the variable *)
	| Node (symb, tlist) -> if List.length tlist = 0 then [] (* if this is a constant term then return an empty list*)
							(* a symbol with arity > 0, return union of variables used int childrens *)
							else List.fold_left (fun a b -> union a (vars b)) [] tlist 
;;

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
let rec mgu (t1:term) (t2:term) = match (t1, t2) with
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

(*  pretending the predicate symbol is a function symbol, *)
let rec att (x:atomic_formula) = match x with
	Atom (a,b) -> Node (a,b)
;;

exception WRONG_USAGE;;

(* Converting a pretending symbol back to predicate *)
let rec tta (x:term) = match x with
	Node(a,b) -> Atom(a,b)
	|_ -> raise WRONG_USAGE
;;


(* Function to print a term *)
let rec print_term x = match x with
	V v -> print_string (v^" ")
	|Node (s,tlist) -> print_string (s^" [");List.iter print_term tlist;print_string "] "
;;

(* Function to print a subst *)
let rec print_subst (x:substitution) = match x with
	[] -> print_string "\n"
	|x::xs -> print_string (fst x); print_string " -> "; print_term (snd x)
;;

(* Solve a set of goal clauses with given program clauses *)
let rec solve (goals:goal) (prog:program) (mgus:substitution)= match goals with
 [] -> print_subst mgus;true
|g0::rest-> let rec resolve (g:atomic_formula) (pr:program) (mgs:substitution) = (match pr with
			  [] -> false
			  |p::ps -> (match p with
						  	Fact f -> (try 
						  				let unifier = mgu (att f) (att g) in (* mgu of f and g by pretending they are function symbol *)
						  				let newgoal = List.map ((fun a t -> subst t a) unifier) (List.map att rest) in
						  				if solve (List.map tta newgoal) prog (mgs@unifier) then true
						  				else resolve g ps mgs
						  			  with
						  			  | NOT_UNIFIABLE -> resolve g ps mgs;
						  			  )
					   	  | Rule r -> (try
					   	  			    let f = fst r in
				   						let unifier = mgu (att f) (att g) in
				   						let newgoal = union (List.map ((fun a t -> subst t a) unifier) (List.map att rest)) 
				   											(List.map ((fun a t -> subst t a) unifier) (List.map att (snd r))) in
				   						if solve (List.map tta newgoal) prog (mgs@unifier) then true
				   						else resolve g ps mgs
				   					   with
				   					   | NOT_UNIFIABLE -> resolve g ps mgs;
				   					   )
				   		)
		    ) in resolve g0 prog mgus
;;
 