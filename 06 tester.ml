#use "06.ml"
let a = Node ("a", []) (* constant term *)
let b = Node ("b", []) (* constant term *)
let x = V "X";;
let y = V "Y";;

let f1 = Atom ("Loves", [a;b]) (* an atomic formula *)
let f2 = Atom ("Loves", [b;a]) (* an atomic formula *)
let r1 = (Atom ("Loves", [x;y]),[ (Atom("Loves", [y;x])) ] )(* another atomic formula with body *)

let goal = [f1];;
let prog = [Fact f2; Rule r1]

(* should return true *)
solve goal prog;;