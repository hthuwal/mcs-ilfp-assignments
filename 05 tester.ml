#use "05 Baby Propositional Prolog interpreter in OCaml.ml"

(* Atoms *)
let p1 = P "s1";;
let p2 = P "s2";;
let p3 = P "s3";;

(* Facts *)
let f1 : atom * atom list = (P "s1",[]);;
let f2 : atom * atom list = (P "s2",[]);;
let f3 : atom * atom list = (P "s3",[]);;

(* Rules *)
let r1 = (p1, [p2;p3]);;
let r2 = (p2, [p3;p1]);;

(* Program *)
let p1 = [f2; f3; r1];;

(* Goal *)
let g1 = [f1];;
