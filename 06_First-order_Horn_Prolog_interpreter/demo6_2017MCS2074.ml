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


(***** Program 3 ******)

(* Constants *)
let d = Node ("d", []);;
let u = Node ("u", []);;
let t = Node ("t", []);;
let b = Node ("b", []);;
let f = Node ("f", []);;
let a = Node ("a", []);;
let b = Node ("b", []);;
let c = Node ("c", []);;
let xx = Node ("xx", []);;
let yy = Node ("yy", []);;

(* Variables *)
let j_var = V "J";;
let e_var = V "E";;
let f_var = V "F";;
let t_var = V "T";;
let u_var = V "U";;
let k_var = V "K";;
let m_var = V "M";;
let n_var = V "N";;
let x_var = V "X";;
let y_yar = V "Y";;
let z_var = V "Z";;

(* Facts *)
let f1 = Fact (Atom ("h", [j_var; d; u]));;
let f2 = Fact (Atom ("h", [j_var; t; b]));;
let f3 = Fact (Atom ("h", [j_var; f; b]));;

(* Rules *)
let r1 = Rule(
	Atom ("h", [j_var; Node ("p", [e_var; f_var]); Node ("r", [t_var; u_var]) ] ),
	[Atom ("h",[j_var; e_var; t_var]); Atom ("h", [j_var; f_var; u_var])]
);;

let r2 = Rule(
	Atom ("h", [j_var; Node ("n", [k_var]); m_var ] ),
	[Atom ("h", [j_var; k_var; Node ("r",[m_var; n_var])]) ]
);;

let r3 = Rule(
	Atom ("h", [j_var; Node ("m", [k_var]); n_var]),
	[Atom ("h", [j_var; k_var; Node ("r",[m_var; n_var])])]
);;

(* actual program3 *)
let program3 = [
	f1;f2;f3;
	r1;r2;r3
];;

(* queries *)
let q3_1 = [ 
	Atom (
		"h", 
		[
			Node ("list", [a;b;c]); 
		 	Node ("p", [t;z_var]);
			Node ("r", [b;u])
		]
	)
];;

let q3_2 = [
	Atom (
		"h",
		[
			Node ("list", [xx; yy]);
			Node ("n", [Node ("p", [t;f])]);
			u
		]
	)
];;

let q3_3 = [
	Atom (
		"h",
		[
			Node ("list", [xx; yy]);
			Node ("n", [Node ("p", [t; y_yar])]);
			x_var
		]
	)
];;

(***** tests  *****)
let test5 = try prolog q2_1 program2 with | END -> print_string "true\n";;
let test6 = try prolog q2_2 program2 with | END -> print_string "true\n";;
let test7 = try prolog q2_3 program2 with | END -> print_string "true\n";;
let test8 = try prolog q2_4 program2 with | END -> print_string "true\n";;
(* let test9 = try prolog q2_5 program2 with | END -> print_string "true\n";;  (* causes infinite loop *) *)
let test10 = try prolog q2_6 program2 with | END -> print_string "true\n";;
let test11 = try prolog q3_1 program3 with | END -> print_string "true\n";;
let test12 = try prolog q3_2 program3 with | END -> print_string "true\n";;
let test13 = try prolog q3_3 program3 with | END -> print_string "true\n";;
