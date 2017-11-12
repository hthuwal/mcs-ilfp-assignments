#use "06.ml"

(* constant terms *)
let burger = Node ("burger",[]);; 
let sandwich = Node ("sandwich", []);;
let pizza = Node ("pizza", []);;

(* Variable *)
let x = V "X";;

(* Atomic Formulas*)
let f1 = Atom ("food",[burger]);;
let f2 = Atom ("food",[sandwich]);;
let f3 = Atom ("food",[pizza]);;
let f4 = Atom ("lunch",[sandwich]);;
let f5 = Atom ("dinner",[pizza]);;


(* Atomic Formulas with body *)
let r1 = (Atom ("meal",[x]), [ Atom("food",[x])] );;

(* Program *)
let prog = [Fact f2; Fact f1; Fact f3; Fact f4; Fact f5; Rule r1];;

(* Queries *)

(* Is pizza a food *)
let g1 = [Atom ("food",[pizza])];;
prolog g1 prog;; 

(* Which food is meal and lunch *)
let g2 = [Atom ("meal",[x]); Atom ("lunch",[x])];;
prolog g2 prog;;

let g3 = [Atom ("dinner",[sandwich])];;
prolog g3 prog;;