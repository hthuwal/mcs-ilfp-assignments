(* Program 1 *)
let a = P "a";;
let b = P "b";;
let program1 = [Fact a; Fact b];;
let goal1 = [P "a"];;

(* Program 2 *)
let x = P "x";;
let y = P "y";;
let z = P "z";;
let r = (z, [x; y]);;
let program2 = [Fact x; Fact y; Rule r];;
let goal2 = [z;x];;

(* Program 3 *)
let m = P "m";;
let n = P "n";;
let p = P "p";;
let q = P "q";;
let r3_1 = (p,[m]);;
let r3_2 = (q,[p;n]);;
let program3 = [Fact m; Fact n; Rule r3_1; Rule r3_2];;
let goal3 = [q;p];;

(* Program 4 *)
let k = P "k";;
let l = P "l";;
let r4_1 = (k, [l]);;
let r4_2 = (l, [k]);;
let program4 = [Rule r4_1; Rule r4_2];;
let goal4 = [l];;

(* tests *)
let test1 = solve_dfs goal1 program1;;
let test2 = solve_bfs goal1 program1;;

let test3 = solve_dfs goal2 program2;;
let test4 = solve_bfs goal2 program2;;

let test5 = solve_dfs goal3 program3;;
let test6 = solve_bfs goal3 program3;;

let test8 = solve_bfs goal4 program4;;
let test7 = solve_dfs goal4 program4;;

