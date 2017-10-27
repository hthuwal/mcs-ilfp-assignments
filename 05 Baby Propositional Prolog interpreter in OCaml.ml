(* head is also a single atom *)
type atom = P of string;; 

(* rule has head and body *)
type rule = atom * (atom list);;

(* clause is either a fact or a rule*)
type clause = Fact of atom | Rule of rule;;

(* program is a list of clauses *)
type program = clause list;;

(* Goal is a list of atoms *)
type goal = atom list;;


(* Resolve a single goal g with all rules in program .
*  returns a list of tuples where each tuple is of the form
*  (true, body of rule/subgoal) if resolution possible 
*  and (false, []) if resolution is not possible
*)
let rec resolve g program = match program with
[] -> []
|x::xs -> match x with
		  Fact y -> if y = g 
          				then [(true, [])]@resolve g xs 
		  			else [(false, [])]@resolve g xs 
		 |Rule y -> if fst y = g 
          				then [(true, snd y)]@resolve g xs 
		  			else [(false, [])]@resolve g xs 
		  
;;



(* Solve depth first a list of goals using a program *)
let rec solve_dfs goal program = match goal with
[] -> true
|x::xs -> let results = resolve x program in
		  let hc b res = if fst res = true then b || solve_dfs (xs@(snd res)) program else b || false in
		  List.fold_left hc false results
;;


(* Solve bredth first a list of goals using a program *)
let rec solve_bfs goal program = match goal with
[] -> true
|x::xs ->  *)
