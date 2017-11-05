type symbol = string;;
type variable = string;;
type predicate = string;;
type term = V of variable | Node of symbol * term list;; (* A term is either a variable, a constant (0 ary symbol), or a k-ary function symbol with k subterms. *)
type atomic_formula = predicate * term list;; (* An atomic formula is a k-ary predicate symbol followed by k terms. *)
type fact = atomic_formula;;(* A fact has a head but no body *)
type rule = atomic_formula * atomic_formula list;; (* A rule has a head and a body.The head is a single atomic formula.A body is a sequence of atomic formulas. *)
type clause = Fact of fact | Rule of rule;; (*  A clause can either be a fact or a rule. *)
type program = clause list;; (* A program is a set (list) of clauses. *)
type goal = atomic_formula list;;(* A goal is a set (list) of atomic formulas. *)

