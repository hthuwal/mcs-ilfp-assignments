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
[] -> [] (* if no rule left return empty list *)
|x::xs -> match x with
		  Fact y -> if y = g  (* if fact matches the goal then can be resolved*)
          				then [(true, [])]@resolve g xs  (* return true and empty subgoal, try with other rules *)
		  			else [(false, [])]@resolve g xs  (* can't be resolved with this fact return false, try other rules *)

		 |Rule y -> if fst y = g  (* if the head of rule matches the goal can be resolved *)
          				then [(true, snd y)]@resolve g xs (* return true and body of rule as subgoal, try other rules *)
		  			else [(false, [])]@resolve g xs (* can't be resolved with this rule return false, try other rules *)
		  
;;



(* Solve breadth first a list of goals using a program 
* return number of ways resolution is possible
* 0 if resolution is not possible 
*)
let rec solve_bfs goal program = match goal with
[] -> 1 (* if no goal left then we have resolved all goals, return true *)
|x::xs -> let results = resolve x program in (* try this goal with all rules *)
		  let hc b res = if fst res = true then b + solve_bfs (xs@(snd res)) program else b + 0 in
		  List.fold_left hc 0 results (* solve recursively for rest of goals wherever this goal is resolved*)
;;


(* Solve depth first a list of goals using a program 
* return number of ways resolution is possible
* 0 if resolution is not possible 
*)
let rec solve_dfs goal program = match goal with
[] -> 1 (* if no goal left then we have resolved all goals, return true *)
|x::xs -> let results = resolve x program in (* try this goal with all rules *)
		  let hc b res = if fst res = true then b + solve_dfs ((snd res)@xs) program else b + 0 in
		  List.fold_left hc 0 results (* solve recursively for rest of goals wherever this goal is resolved*)
;;
