#use "05 Baby Propositional Prolog interpreter in OCaml.ml"

(* Facts *)
let f1 = P "s1";;
let f2 = P "s2";;
let f3 = P "s3";;

(* Rules *)
let r1 = (f1, [f2;f3]);;
let r2 = (f2, [f3;f1]);;

(* Program *)
let prog1 = [Fact f2; Fact f3; Rule r1];;

(* Goal *)
let g1 = [f1; P "s4"];;

Printf.printf "%i\n" (solve_dfs g1 prog1);;
Printf.printf "%i\n" (solve_bfs g1 prog1);;

(* Another set of test cases *)
let fs1 = Fact (P "s");;
let fs2 = Fact (P "t");;
let fs3 = Fact (P "u");;

let rs1 = Rule (P "q", [P "s"; P "t"]);;
let rs2 = Rule (P "q", [P "t";]);;

let rs3 = Rule (P "r", [P "z"]);;

let ps1 = [fs1; fs2; fs3; rs1; rs2];;

let gs1 = [P "s"; P "q"];;

let gs2 = [P "z"];;

(* Should return true *)
Printf.printf "%i\n" (solve_dfs gs1 ps1);;
Printf.printf "%i\n" (solve_bfs gs1 ps1);;


(* Should return false *)
Printf.printf "%i\n" (solve_dfs gs2 ps1);;
Printf.printf "%i\n" (solve_bfs gs2 ps1);;


(* Should return true *)
Printf.printf "%i\n" (solve_dfs [] []);;
Printf.printf "%i\n" (solve_bfs [] []);;

Printf.printf "%i\n" (solve_dfs [] ps1);;
Printf.printf "%i\n" (solve_bfs [] ps1);;


(* Should return false *)
Printf.printf "%i\n" (solve_dfs gs1 []);;
Printf.printf "%i\n" (solve_bfs gs1 []);;
