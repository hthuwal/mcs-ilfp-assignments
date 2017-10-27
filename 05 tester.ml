#use "05 Baby Propositional Prolog interpreter in OCaml.ml"

(* Facts *)
let f1 = P "s1";;
let f2 = P "s2";;
let f3 = P "s3";;

(* Rules *)
let r1 = (f1, [f2;f3]);;
let r2 = (f2, [f3;f1]);;

(* Program *)
let p1 = [Fact f2; Fact f3; Rule r1];;

(* Goal *)
let g1 = [f1];;
