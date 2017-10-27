type atom = P of string;; (* head is also a single atom *)
type rule = atom * (atom list);; (* rule has head and body *)
type clause = Fact of atom | Rule of rule;;(* clause is either a fact *)
type program = clause list;;
(* Goal is a list of atoms *)


