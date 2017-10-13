(***** Functions of set required *****)
let emptySet = [];;

(* Inert element a in set s *)
let insert a s = if List.mem a s then s else s @ a::[] ;; 

(* Set Union *)
let rec union s1 s2 = match s1 with 
                      [] -> s2
                      | x::xs -> insert x (union xs s2)
                      ;;

(***** Declaring  new data type for proposition *****)
type prop = P of string 
           | T 
           | F
           | Not of prop
           | And of prop * prop
           | Or of prop * prop
           | Implies of prop * prop
           ;;

let p1 = P("Harish") ;;
let p2 = P("Chandra") ;;
let p3 = P("Thuwal") ;;
let p4 = T ;;
let p5 = F ;;

(* let prop1 = Not(Implies(p1, Not(Or(p2,p3))));; *)
let prop1 = And (p1, Or(p2,p3));;
let prop2_taut = Implies(prop1,prop1);; (*A->A is always true*)
let prop3_contr = And(Or(p1,p2),And(Not p1,Not p2));;


(* Function to return a string representation of a proposition *)
let rec string_repr p = match p with
                        T -> "T"
                      | F -> "F"
                      | P str -> "P(" ^ str ^ ")"
                      | Not (p1) -> "~[" ^ string_repr p1  ^ "]"
                      | And (p1,p2) -> "[" ^ string_repr p1  ^ " ^ " ^ string_repr p2 ^ "]"  
                      | Or (p1,p2) -> "[" ^ string_repr p1  ^ " v " ^ string_repr p2 ^ "]"
                      | Implies (p1,p2) -> "[" ^ string_repr p1  ^ " -> " ^ string_repr p2 ^ "]"
                      ;;

print_string "\nprop1 is : "; print_endline (string_repr(prop1));;
print_string "\nprop2_taut (prop1->prop1) is : "; print_endline (string_repr(prop2_taut));;
print_string "\nprop3_contr is : "; print_endline (string_repr(prop3_contr));print_string "\n";;



(***** 1. Height : height of the operator tree, counting from 0 *****)
let rec height p = match p with
                   T -> 0
                 | F -> 0
                 | P str -> 0
                 | Not (p1) -> 1 + height p1
                 | And (p1,p2) -> 1 + max (height p1) (height p2)
                 | Or (p1,p2) -> 1 + max (height p1) (height p2)
                 | Implies (p1,p2) -> 1 + max (height p1) (height p2)
                 ;;

print_string "\n\n";
print_string "Height of prop1 is : " ; print_int (height prop1) ; print_string "\n";;
print_string "Height of prop2_taut is : " ; print_int (height prop2_taut) ; print_string "\n";;
print_string "Height of prop3_contr is : " ; print_int (height prop3_contr) ; print_string "\n";;



(***** 2. Size : number of nodes in the operator tree*****)
let rec size p = match p with
                   T -> 1
                 | F -> 1
                 | P str -> 1
                 | Not (p1) -> 1 + size p1
                 | And (p1,p2) -> 1 + size p1 + size p2
                 | Or (p1,p2) -> 1 + size p1 + size p2
                 | Implies (p1,p2) -> 1 + size p1 + size p2
                 ;;

print_string "\n\n";
print_string "Size of prop1 is : " ; print_int (size prop1) ; print_string "\n";;
print_string "Size of prop2_taut is : " ; print_int (size prop2_taut) ; print_string "\n";;
print_string "Size of prop3_contr is : " ; print_int (size prop3_contr) ; print_string "\n";;



(***** 3. Letters the set of propositional variables that appear in a proposition.*****)
let rec letters p = match p with
                    T -> emptySet
                   |F -> emptySet
                   |P str -> [str]
                   |Not(p1) -> letters p1
                   |And(p1,p2) -> union (letters p1) (letters p2)
                   |Or(p1,p2) -> union (letters p1) (letters p2)
                   |Implies(p1,p2) -> union (letters p1) (letters p2)
                   ;;

print_string "\n\n";
print_string "Propositional Variables in prop1 are : ";; List.map (Printf.printf "%s ") (letters(prop1));; print_string "\n";;
print_string "Propositional Variables in prop2_taut are : ";; List.map (Printf.printf "%s ") (letters(prop2_taut));; print_string "\n";;
print_string "Propositional Variables in prop3_contr are : ";; List.map (Printf.printf "%s ") (letters(prop3_contr));; print_string "\n";;



(***** 4. Truth : which evaluates a proposition with respect to a given truth assignment to the propositional letters*****)

(*truth assignment*)
let rho str = match str with
              "Harish" -> false
              |"Chandra" -> true
              |"Thuwal" -> true
              | _ -> false
              ;;

let rec truth p rho = match p with
                      T -> true
                     |F-> false
                     |P str -> rho str
                     |Not p1 -> not (truth p1 rho)
                     |And (p1,p2) -> (truth p1 rho) && (truth p2 rho)
                     |Or (p1,p2) -> (truth p1 rho) || (truth p2 rho)
                     |Implies(p1,p2) -> if (truth p1 rho) then (truth p2 rho) else true
                     ;;

print_string "\n\n";
print_string "rho Harish : "; if ( rho "Harish" ) then print_string "True!\n" else print_string "False!\n";;
print_string "rho Chandra : "; if ( rho "Chandra" ) then print_string "True!\n" else print_string "False!\n";;
print_string "rho Thuwal : "; if ( rho "Thuwal" ) then print_string "True!\n" else print_string "False!\n";;print_string "\n";;

print_string "Truth Value of prop1 is : "; if ( truth prop1 rho ) then print_string "True!\n" else print_string "False!\n";;
print_string "Truth Value of prop2_taut is : "; if ( truth prop2_taut rho ) then print_string "True!\n" else print_string "False!\n";;
print_string "Truth Value of prop3_contr is : "; if ( truth prop3_contr rho ) then print_string "True!\n" else print_string "False!\n";;



(***** 5. NNF : which converts a proposition into negation normal form *****)
let rec nnf p = match p with
                T -> p
              | F -> p
              | P str -> p
              | And (p1,p2) -> And (nnf p1, nnf p2)
              | Or (p1,p2) -> Or (nnf p1, nnf p2)
              | Implies (p1,p2) -> Or (nnf (Not p1),nnf p2)
              | Not p1 -> (match p1 with
                          T -> F
                        | F -> T
                        | P str -> p
                        | And (q1, q2) -> Or (nnf (Not q1), nnf (Not q2))
                        | Or (q1, q2) -> And (nnf (Not q1), nnf (Not q2))
                        | Not q -> nnf q
                        | Implies (q1, q2) -> And (nnf q1, nnf(Not q2)) )
              ;;

let nnf_prop2_taut = nnf prop2_taut;; 

print_string "\n\n";
print_string "Negation Normal Form of prop2_taut is : "; print_string (string_repr(nnf_prop2_taut)); print_string "\n";;
print_string "Truth Value of nnf of prop2_taut is : "; if ( truth nnf_prop2_taut rho ) then print_string "True!\n" else print_string "False!\n";;




(***** 6. CNF : Converts a proposition into conjunctive normal form (POS) as a (conjunctive) set of clauses , where each clause is
 considered as a (disjunctive) set of literals *****)

(* Converts an nnf to cnf *)
let rec cnf_f_nnf p = match p with
              T -> T
            | F -> F
            | P str -> p
            | Not p1 -> p
            | And (p1,p2) -> And (cnf_f_nnf p1, cnf_f_nnf p2)
            | Or (p1,And(q1,q2)) ->  cnf_f_nnf (And(Or(cnf_f_nnf p1,cnf_f_nnf q1), Or(cnf_f_nnf p1,cnf_f_nnf q2)))
            | Or (And(q1,q2),p1) ->  cnf_f_nnf (And(Or(cnf_f_nnf q1,cnf_f_nnf p1), Or(cnf_f_nnf q2,cnf_f_nnf p1)))
            | Or (p1,p2) -> Or(cnf_f_nnf p1, cnf_f_nnf p2)
            | _ -> p
            ;;

(* First convert to nnf then to cnf *)
let cnf p = cnf_f_nnf(nnf(p));;

(* Fuction to string representation of propositions containing only literals, not and disjunction *)
let rec repr_or_terms p = match p with|
                      T -> "T"
                    | F -> "F"
                    | P str -> "P("^str^")"
                    | Or (p1,p2) -> (repr_or_terms p1)^" v "^(repr_or_terms p2)
                    | Not p1 -> "~[" ^ string_repr p1  ^ "]"
                    | _ -> ""
                    ;;

(* Creates a set of Sum terms from POS // splits proposition at ands into a set of clauses*)
let rec split_at_and p = match p with 
                            And (p1,p2) -> union (split_at_and p1) (split_at_and p2)
                          | literal -> [literal]
                          ;;

let cnf_prop2_taut =  cnf prop2_taut;;
let cnf_prop1 = cnf prop1;;
let cnf_prop3_contr = cnf prop3_contr;;

print_string "\n\n";
print_string "CNF form of prop2_taut contains the Conjuction of following clauses :\n";;
List.iter print_endline (List.map repr_or_terms (split_at_and(cnf_prop2_taut)));;
print_string "\nTruth Value of cnf of prop1 is : "; if ( truth cnf_prop1 rho ) then print_string "True!\n" else print_string "False!\n";;
print_string "Truth Value of cnf of prop2_taut is : "; if ( truth cnf_prop2_taut rho ) then print_string "True!\n" else print_string "False!\n";;
print_string "Truth Value of cnf of prop3_contr is : "; if ( truth cnf_prop3_contr rho ) then print_string "True!\n" else print_string "False!\n";;


(***** 7. DNF : which converts a proposition into disjunctive normal form (SOP) as a (disjunctive) set of terms , where each term is a set of
literals *****)
 
(* Convert an nnf to dnf *)
let rec dnf_from_nnf p = match p with
              T -> T
            | F -> F
            | P str -> p
            | Not p1 -> p
            | Or (p1,p2) -> Or (dnf_from_nnf p1, dnf_from_nnf p2)
            | And (p1,Or(q1,q2)) ->  dnf_from_nnf (Or(And(dnf_from_nnf p1,dnf_from_nnf q1), And(dnf_from_nnf p1,dnf_from_nnf q2)))
            | And (Or(q1,q2),p1) ->  dnf_from_nnf (Or(And(dnf_from_nnf q1,dnf_from_nnf p1), And(dnf_from_nnf q2,dnf_from_nnf p1)))
            | And (p1,p2) -> And(dnf_from_nnf p1, dnf_from_nnf p2)
            | _ -> p
            ;;

(* First convert to nnf then to dnf *)
let dnf p = dnf_from_nnf(nnf(p));;

(* Fuction to give string representation of propositions containing only literals, not and conjuction *)
let rec repr_and_terms p = match p with|
                        T -> "T"
                      | F -> "F"
                      | P str -> "P("^str^")"
                      | And (p1,p2) -> (repr_and_terms p1)^" ^ "^(repr_and_terms p2)
                      | Not p1 -> "~[" ^ string_repr p1  ^ "]"
                      | _ -> ""
                      ;;

(* Creates a set of product terms from SOP // splits proposition at ors into a set of clauses*)
let rec split_at_or p = match p with 
                            Or (p1,p2) -> union (split_at_or p1) (split_at_or p2)
                          | literal -> [literal]
                          ;;

let dnf_prop2_taut =  dnf prop2_taut;;
let dnf_prop1 = dnf prop1;;
let dnf_prop3_contr = dnf prop3_contr;;

print_string "\n\n";
print_string "DNF form of prop2_taut contains the Disjunction of following clauses :\n";;
List.iter print_endline (List.map repr_and_terms (split_at_or(dnf_prop2_taut)));;
print_string "\nTruth Value of dnf of prop1 is : "; if ( truth dnf_prop1 rho ) then print_string "True!\n" else print_string "False!\n";;
print_string "Truth Value of dnf of prop2_taut is : "; if ( truth dnf_prop2_taut rho ) then print_string "True!\n" else print_string "False!\n";; 
print_string "Truth Value of dnf of prop3_contr is : "; if ( truth dnf_prop3_contr rho ) then print_string "True!\n" else print_string "False!\n"; print_string "\n";;


(***** 8. is tautology : which checks if a proposition is a tautology*****)

(* A Conjunctive Normal Form is a statement which is a conjunction of a series of clauses where each clause is itself a disjunction of a series of literals.
   Such a statement is a tautology if and only if each and every clause is a tautology. A clause is a tautology if and only if either 
   (1) one of the literals is a true valued constant or 
   (2) the clause contains both a literal and its own negation. *)

(* Function to check a clause/Sum term of CNF/POS is a tautology or not*)
let isClauseTautotlogy p = let literals = split_at_or p in  (* Extracting literals from each sum term *)
                          List.mem T literals  (* Check if T present*)
                          || List.fold_left (fun c a -> (List.mem (nnf(Not a)) literals) || c) false literals (*Check presence of a literal and its negations*)
                          ;;

(* Checking Given Proposition is Tautology or not *)
let isTautology p = let p_cnf = cnf p in (* Converting to CNF*)
                    let clauses_of_p_cnf = split_at_and p_cnf in (* Creating Set of clauses/Sum terms of CNF/POS*)
                    List.fold_left (fun c a -> isClauseTautotlogy a && c) true clauses_of_p_cnf;;  (* Check if each clause is a tautology*)

print_string "\n\nprop1 is ";; if isTautology prop1 then print_string "a tautology\n" else print_string "not a tautology\n";;
print_string "prop2_taut is ";; if isTautology prop2_taut then print_string "a tautology\n" else print_string "not a tautology\n";;
print_string "prop3_contr is ";; if isTautology prop3_contr then print_string "a tautology\n" else print_string "not a tautology\n";;



(***** 9. is Contradiction : which checks if a proposition is a contradiction.*****)
(* A contradiction implies and is implied by the negation of a tautology: *)
let isContradiction p = isTautology (Not p);;

print_string "\n\nprop1 is ";;if isContradiction prop1 then print_string "a contradiction\n" else print_string "not a contradiction\n";;
print_string "prop2_taut is ";;if isContradiction prop2_taut then print_string "a contradiction\n" else print_string "not a contradiction\n";;
print_string "prop3_contr is ";;if isContradiction prop3_contr then print_string "a contradiction\n" else print_string "not a contradiction\n";;



(***** 10. is Satisfiable : which checks if a proposition is satisfiable *****)

(* proposition is satisfiable iff it is not a Contradiction *)
let isSatisfiable p = not (isContradiction p);;

print_string "\n\nprop1 is ";; if isSatisfiable prop1 then print_string "Satisfiable\n" else print_string "not Satisfiable\n";;
print_string "prop2_taut is ";; if isSatisfiable prop2_taut then print_string "Satisfiable\n" else print_string "not Satisfiable\n";;
print_string "prop3_contr is ";; if isSatisfiable prop3_contr then print_string "Satisfiable\n" else print_string "not Satisfiable\n";;


(***** 11. is Equivalent : which checks if two propositions are logically equivalent *****)
(* p1 is equivalent to p2 if both implies p1 p2 and implies p2 p1 are tautologies*)
let isEquivalent p1 p2 = isTautology (Implies (p1,p2)) && isTautology (Implies(p2,p1));;

print_string "\n\ncnf_prop1 and nnf_prop1 are ";;
if isEquivalent (cnf prop1) (nnf prop1) then print_string "Equivalent\n" else print_string "Not Equivalent\n";;
print_string "prop2_taut and prop3_contr are ";;
if isEquivalent prop2_taut prop3_contr then print_string "Equivalent\n" else print_string "Not Equivalent\n";;

(***** 12. entails : which checks if the second proposition is a logical consequence of the first proposition. *****)
let entails p1 p2 = isTautology (Implies (p1,p2));;
print_string "\n\nprop1 entails nnf_prop1 is ";;
if entails (cnf prop1) (nnf prop1) then print_string "true\n" else print_string "false\n";;
print_string "prop3_contr entails prop2_taut is ";;
if entails prop3_contr prop2_taut then print_string "true\n" else print_string "false\n";;