let sub1 = [("P", V "R")];;

let sub2 = [("Y", Node("g",[V"Z"; V"X"]))];;

let sub3 = substitution_composition [("Y", Node("2",[]))] [("X", V "Y")];;

let sub4 = substitution_composition [("X", Node("*",[V "Y"; V "Y"]))] [("Y", V "Z")];;

let wooo = Node ("g", [ V"X"; Node("*", [ V "Y"; Node("*", [V"X"; V"Y"])] ) ]);;

let sub5 = substitution_composition ( substitution_composition [("X", V "M")] [("Y", V "N")] ) [("Z", wooo)];;

let sub6 = substitution_composition ( substitution_composition [("Z", wooo)] [("X", V "M")] ) [("Y", V "N")];; 

let sub7 = [("X", V "I")];;

let sub8 = [("Y", V "J")];;

let mgu_to_list m = m;;

