#use "06.ml"
let a = Node ("a", []);; (* constant term *)
let b = Node ("b", []);; (* constant term *)
let c = Node ("c", []);; (* constant term *)
let d = Node ("d", []);; (* constant term *)

let x = V "X";;
let y = V "Y";;

let f1 = Atom ("Loves", [a;b]) (* an atomic formula *)
let f2 = Atom ("Loves", [b;a]) (* an atomic formula *)
let f3 = Atom ("Loves", [a;c])
let f4 = Atom ("Loves", [a;d])

let r1 = (Atom ("Loves", [x;y]),[ (Atom("Loves", [y;x])) ] )(* another atomic formula with body *)

let goal = [f1];;
let prog = [Fact f1; Rule r1];;

(* should return true *)
(* Printf.printf ("%b") (solve goal prog []);; *)
Printf.printf (" %b\n") (solve [Atom ("Loves",[x;b])] prog []);;

let goal1 = [ Atom ("Loves",[a;x])];;
let prog1 = [Fact f1; Fact f3; Fact f4];;
solve goal1 prog1 [];;