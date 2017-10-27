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


