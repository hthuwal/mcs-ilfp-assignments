#use "04_Representing_Terms_Substitutions_and_Unification.ml"

let print_bool b = if b then print_string "true\n" else print_string "false\n";;
let rec print_list = function
  | [] -> print_endline ""
  | hd::tl -> print_string (hd ^ " "); print_list tl;;

(* Sanjana *)
let sig1 = [("X",0);("Y",0);("f",1);("g",2);("h",3);("*",2)];;
let sig2 = [("X",0);("Y",0);("Z",0);("f",1);("g",2);("f",3);("*",2)];;
let term1 = (Node ("f",[V "X"]));;
let term2 = (Node ("g",[V "X";Node("h",[Node("f",[V "X"]);V "Y"])]));;
let term3 = (Node ("g",[V "X";Node("*",[V "Y";Node ("*",[V "X";V "Y"])])]));;

let term4 = (Node("g",[V "X";Node("*",[V "Y";V "X"])]));;
let term5 = (Node("g",[V "Z";Node("*",[V "X";V "Z"])]));;
let term6 = (Node("g",[V "Z";Node("g",[V "X";V "Z"])]));;

let (sub1:substitution) = [("X",Node("*",[V "Y";V "Y"]))]
let (sub2:substitution) = [("Y", V "Z")];;
let (csub) = substitution_composition sub1 sub2;;

(* Saurabh92 *)
let ss1 = [("a", 0); ("f", 2); ("g", 1); ("b", 0)];;
let ss2 = [("b", -1); ("f", 2); ("g", 1)];;
let ss3 = [("d", 5); ("f", 2); ("g", 3)];;
let ss4 = [("a", 0); ("f", 2); ("g", 1); ("g", 1)];;
let ss5 = [] 

let t1 = V "var1";;
let t2 = Node ("z", []);;
let t3 = Node ("f", [Node ("a", []); Node ("b", [])]);;
let t4 = Node ("g", [t2]);;
let t5 = Node ("f", [Node ("a", []);
             Node ("y", [V "var2"; Node ("c", [])])]);;
let t6 = Node ("f", [Node ("a", []);
             Node ("y", [Node ("b", []);
                 Node ("c", [Node ("d", [])])])]);;
let t7 = Node ("f", [V "var1";
             Node ("y", [V "var2";
                 Node ("c", [V "var3"])])]);;

let s1 = [("var1", Node ("m", [])); ("var2", Node("n", [])) ];;
let s2 = [("var2", Node ("i", [])); ("var3", Node("j", [])) ];;


(* Sanjana *)
print_string "check_sig sig1 : ";print_bool (check_sig sig1);;
print_string "check_sig sig2 : ";print_bool (check_sig sig2);;
print_string "wfterm term1 sig1 : ";print_bool (wfterm term1 sig1);;
print_string "wfterm term2 sig1 : ";print_bool (wfterm term2 sig1);;
print_string "wfterm term3 sig1 : ";print_bool (wfterm term3 sig1);;
print_string "wfterm term4 sig1 : ";print_bool (wfterm term4 sig1);;
print_string "wfterm term5 sig1 : ";print_bool (wfterm term5 sig1);;
print_string "wfterm term6 sig1 : ";print_bool (wfterm term6 sig1);;
print_string "ht term1 : ";print_int (ht term1);print_endline "";;
print_string "ht term2 : ";print_int (ht term2);print_endline "";;
print_string "ht term3 : ";print_int (ht term3);print_endline "";;
print_string "ht term4 : ";print_int (ht term4);print_endline "";;
print_string "ht term5 : ";print_int (ht term5);print_endline "";;
print_string "ht term6 : ";print_int (ht term6);print_endline "";;
print_string "size term1 : ";print_int (size term1);print_endline "";;
print_string "size term2 : ";print_int (size term2);print_endline "";;
print_string "size term3 : ";print_int (size term3);print_endline "";;
print_string "size term4 : ";print_int (size term4);print_endline "";;
print_string "size term5 : ";print_int (size term5);print_endline "";;
print_string "size term6 : ";print_int (size term6);print_endline "";;
print_string "vars term1 : ";print_list (vars term1);;
print_string "vars term2 : ";print_list (vars term2);;
print_string "vars term3 : ";print_list (vars term3);;
print_string "vars term4 : ";print_list (vars term4);;
print_string "vars term5 : ";print_list (vars term5);;
print_string "vars term6 : ";print_list (vars term6);;
subst term3 sub1;;

(* Saurabh2 *)
print_string "check_sig ss1 : ";print_bool (check_sig ss1);;
print_string "check_sig ss2 : ";print_bool (check_sig ss2);;
print_string "check_sig ss3 : ";print_bool (check_sig ss3);;
print_string "check_sig ss4 : ";print_bool (check_sig ss4);;
print_string "check_sig ss5 : ";print_bool (check_sig ss5);;
print_string "wfterm t1 ss1 : ";print_bool (wfterm t1 ss1);;
print_string "wfterm t2 ss1 : ";print_bool (wfterm t2 ss1);;
print_string "wfterm t3 ss1 : ";print_bool (wfterm t3 ss1);;
print_string "wfterm t4 ss1 : ";print_bool (wfterm t4 ss1);;
print_string "wfterm t5 ss1 : ";print_bool (wfterm t5 ss1);;
print_string "wfterm t6 ss1 : ";print_bool (wfterm t6 ss1);;
print_string "wfterm t7 ss1 : ";print_bool (wfterm t7 ss1);;
print_string "ht t1 : ";print_int (ht t1);print_endline "";;
print_string "ht t2 : ";print_int (ht t2);print_endline "";;
print_string "ht t3 : ";print_int (ht t3);print_endline "";;
print_string "ht t4 : ";print_int (ht t4);print_endline "";;
print_string "ht t5 : ";print_int (ht t5);print_endline "";;
print_string "ht t6 : ";print_int (ht t6);print_endline "";;
print_string "ht t7 : ";print_int (ht t7);print_endline "";;
print_string "size t1 : ";print_int (size t1);print_endline "";;
print_string "size t2 : ";print_int (size t2);print_endline "";;
print_string "size t3 : ";print_int (size t3);print_endline "";;
print_string "size t4 : ";print_int (size t4);print_endline "";;
print_string "size t5 : ";print_int (size t5);print_endline "";;
print_string "size t6 : ";print_int (size t6);print_endline "";;
print_string "size t7 : ";print_int (size t7);print_endline "";;
print_string "vars t1 : ";print_list (vars t1);;
print_string "vars t2 : ";print_list (vars t2);;
print_string "vars t3 : ";print_list (vars t3);;
print_string "vars t4 : ";print_list (vars t4);;
print_string "vars t5 : ";print_list (vars t5);;
print_string "vars t6 : ";print_list (vars t6);;
print_string "vars t7 : ";print_list (vars t7);;