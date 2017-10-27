(* head is also a single atom *)
type atom = P of string;; 

(* 
   Clause can either be a fact or a rule
   rule has head and body 
   Empty body implies a fact
*)

type clause = atom * (atom list);; 

(* Program is a set of clauses *)
type program = clause list;;

(* Goal is a list of atoms *)


(* Resolve a single goal g with all rules in program .
*  returns a list of tuples where each tuple is of the form
*  (true, body of rule/subgoal) if resolution possible 
*  and (false, []) if resolution is not possible
*)
let rec resolve g program = match program with
[] -> []
|x::xs -> if fst x = g 
          	then [(true, snd x)]@resolve g xs 
		  else [(false, [])]@resolve g xs 
;;

(* let rec solve goal program = match goal with
[] -> true
|x::xs ->  *)
