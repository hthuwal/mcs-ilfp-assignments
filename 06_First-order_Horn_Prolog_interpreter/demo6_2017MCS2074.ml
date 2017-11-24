(***** Program 2 *****)

(* Constants *)
let krishna = Node ("krishna", []);;
let pandu = Node ("pandu", []);;
let arjun = Node ("arjun", []);;
let nakul = Node ("nakul", []);;
let kunti = Node ("kunti", []);;
let madri = Node ("madri", []);;
let draupadi = Node ("draupadi", []);;

(* Variables *)
let x = V "X";;
let y = V "Y";;
let z = V "Z";;

(* Facts *)
let male_pandu = Fact (Atom ("male", [pandu])) ;;
let male_arjun = Fact (Atom ("male", [arjun])) ;;
let male_nakul = Fact (Atom ("male", [nakul])) ;;

let female_kunti = Fact (Atom ("female", [kunti])) ;;
let female_madri = Fact (Atom ("female", [madri])) ;;
let female_draupadi = Fact (Atom ("female", [draupadi])) ;;

let married_pandu_kunti = Fact (Atom ("married", [pandu;kunti]));;
let married_pandu_madri = Fact (Atom ("married", [pandu;madri]));;
let married_arjun_draupadi = Fact (Atom ("married", [arjun;draupadi]));;
let married_nakul_draupadi = Fact (Atom ("married", [nakul;draupadi]));;

(* Rules *)
let wife_rule = Rule (
	Atom ("wife", [x;y]), 
	[Atom ("married", [x;y]); Atom ("male", [x]); Atom ("female",[y])]
);;

let cowife_rule = Rule (
	Atom ("cowife", [x;y]),
	[Atom ("married", [z;x]); Atom ("married", [z;y]); Atom ("female", [x]); Atom ("female", [y])]
);;


let husband_rule = Rule (
	Atom ("husband", [x;y]),
	[Atom ("married", [y;x]); Atom ("male", [y]); Atom ("female", [x])]
);;

let cohusband_rule = Rule (
	Atom ("cohusband", [x;y]),
	[Atom ("married", [x;z]); Atom ("married", [y;z]); Atom ("male", [x]); Atom("male", [y])]
);;

(* actual program2 *)
let program2 = [
	male_pandu; 
	male_arjun; 
	male_nakul; 
	female_kunti; 
	female_madri; 
	female_draupadi; 
	married_pandu_kunti; 
	married_pandu_madri; 
	married_arjun_draupadi; 
	married_nakul_draupadi; 
	wife_rule; 
	cowife_rule; 
	husband_rule; 
	cohusband_rule
];;

(* queries *)
let q2_1 = [Atom ("male", [krishna])];;
let q2_2 = [Atom ("married", [arjun; x])];;
let q2_3 = [Atom ("married", [x;draupadi])];;
let q2_4 = [Atom ("married", [x;y])];;
let q2_5 = [Atom ("cohusband", [arjun;nakul]); Atom ("cowife", [madri; kunti])];;
let q2_6 = [Atom ("cowife", [x;kunti])];;


(***** tests  *****)
let test5 = try prolog q2_1 program2 with | END -> print_string "true\n";;
let test6 = try prolog q2_2 program2 with | END -> print_string "true\n";;
let test7 = try prolog q2_3 program2 with | END -> print_string "true\n";;
let test8 = try prolog q2_4 program2 with | END -> print_string "true\n";;
(* let test9 = try prolog q2_5 program2 with | END -> print_string "true\n";;  (* causes infinite loop *) *)
let test10 = try prolog q2_6 program2 with | END -> print_string "true\n";;