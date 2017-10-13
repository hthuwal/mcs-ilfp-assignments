(*********** Sets uing OCaml List *************)

(* Representation invariant: a set is represented as a list without duplicates. *)

print_string "\n\t\t Sets using OCaml List\n\n";;
let a_set = [1;2;3;4];;
let b_set = [2;4;6;8];;

(* Print a set *)
let rec print s = match s with
                  [] -> print_string ("\n")
                  |x::xs -> print_int x; print_string " "; print xs 
                  ;;

print_string "a_set : " ; print a_set;;
print_string "b_set : " ; print b_set;;
print_string "\n"



(* Inert element a in set s *)
let insert a s = if List.mem a s then s else s @ a::[] ;; 
(*      Proof of Preservance of Representational Invariance
*  
*  Base Case: when list is empty it is an emptyset
*  Induction Hypothesis: Let S a list with k elements be a set i.e, it contains k unique elements
*  Induction Step: Case 1: Add a new distinct element p
*                          In this case the List.mem a returns false as p is not a member of set 
*                          therefore it is added to list and the list now contains k+1 unique elements 
*                          Thus S now containse k+1 ddistinct elements i.e, it is a set with k+1 elements
*                  Case 2: The element p already present in list S
*                          In this case List.mem a returns ture as it already present in the set S 
*                          therefore it is not appended to the list S and S remains a set.
*  
*  Therefore the insert operations maintains the represenational invariant the resultant list after insert
*  operation remains a Set i.e no duplicates
*)

Printf.printf "Inserting %d to b_set\n" 0;;
let b_set = insert 0 b_set;;
print_string "b_set : " ; print b_set;;

Printf.printf "Inserting %d to b_set\n" 6;;
let b_set = insert 6 b_set;;
print_string "b_set : " ; print b_set;;
print_string "\n";;



(**** 1. EmptySet ****) 
let emptySet = [];;
(*
* Empty Set containse no elements. therefore it satisfies the representational invariant 
* that it is a list with no duplicate elements (a set)  
*)
let isEmpty s = if s = emptySet then true else false;;

if isEmpty [] then print_string "This is a empty set\n\n";;



(**** 2. Member x s ****)
(* let member x s = if List.mem x s then true else false ;; *)
let rec member y s = match s with
                     [] -> false
                     |x::xs -> if y = x then true else member y xs
                     ;; 

let x = 5;;
let y = 2;;
if member x a_set then Printf.printf "Yes, %d belongs to the set a_set\n" x  else  Printf.printf "No, %d does not belongs to set the a_set\n" x ;;
if member y a_set then Printf.printf "Yes, %d belongs to the set a_set\n" y  else  Printf.printf "No, %d does not belongs to set the a_set\n" y ;;
print_string "\n";;



(**** 3 Cardinality ****)
(* let cardinality s = List.length s;; *)
let rec cardinality s = match s with
                    [] -> 0
                    | x::xs -> 1 + cardinality xs
                    ;;

let card = cardinality a_set;;
Printf.printf "Cardinality of a_set is %d\n\n" card 



(**** 4. Union ****)
let rec union s1 s2 = match s1 with 
                      [] -> s2
                      | x::xs -> insert x (union xs s2)
                      ;;

(*      Proof of Preservance of Representational Invariance
*
* Given that the inputs s1 and s2 satisfy the representational invariance i.e. they have no duplicates (they are sets)
* Induction on cardinality of s1
* Base Case: s1 is EmptySet (cardinality 0)
*            s1 U s2 = EmptySet U s2 = s2 ( because A U emptyset is A )
*            s2 is a set (given)
* 
* IH: let sk U s2 gives a set i.e. the operation preserves the representational invariance i.e all elements in resultant list are unique
*     where sk is a set with cardinality k
*     
* IS: sk+1 is a set with cardinality k+1
*     sk+1 = p::sk //in the above function p is the first element and ths sk contains remaining k elements
*     for sk+1 the above method returns 
*     insert p (uninon sk s2)
*     insert p s'      ( s' = sk U s2 using induction hypothesis s' preserves representational invariance i.e s' is a set )
*
*     therefore sk+1 is a set ( because the insert operation preserves the representaional invariance proved above line #21)
* 
* Hence the union operation preserves the representational invariance of the inputs i.e the union contains no duplicates
*)

print_string "a_set U b_set : " ; print (union a_set b_set) ;;
print_string "\n";;



(**** 5. Intersection ****)
let rec intersection s1 s2 = match s1 with
                             [] -> []
                             | x::xs -> if member x s2 then x :: (intersection xs s2)
                                        else intersection xs s2
                             ;;
(*      Proof of Preservance of Representational Invariance
*
* Given that the inputs s1 and s2 satisfy the representational invariance i.e. they have no duplicates (they are sets)
* Induction on Cardinality of s1
* BaseCase: s1 is EmptySet (cardinality 0)
*           s1 intersection s2 = EmptySet intersection s2 = EmptySet  (because emptySet intersection anything is emptySet)
*           EmptySet trivially satisfies the representational invariance i.e it has no duplicates i.e it is a set
*       
* IH: let sk intersection s2 gives a set i.e. the operation preserves the representational invariance i.e all elements in resultant list are unique
*     where sk is a set with cardinality k
* 
* IS: sk+1 is a set with cardinality k+1
*     sk+1 = p::sk //in the above function p is the first element and thus sk contains remaining k elements none of which is p
*     for sk+1 two cases arise:
*       s' = sk intersection s2 which preserves representational invariance (using induction hypothesis)
*       Case 1: p is in s2, then above code gives p::(s')
*               where p cannot be in s' because it was not in sk
*               therefore p::s' also preserves representational invariance
*       Case 2: p is not in s2, then above code gives s' hich is a set using IH
*             
* Hence the intersection operation preserves the representational invariance of the inputs i.e the intersection contains no duplicates
*)

print_string "Their intersection : " ; print (intersection a_set b_set) ;;
print_string "\n";;



(**** 6. Set Difference ****)
let rec difference s1 s2 = match s1 with
                           [] -> []   
                           | x::xs -> if not (member x s2) then x :: (difference xs s2)
                           else  difference xs s2
                           ;;
(*      Proof of Preservance of Representational Invariance
*
* Given that the inputs s1 and s2 satisfy the representational invariance i.e. they have no duplicates (they are sets)
* Induction on Cardinality of s1
* BaseCase: s1 is EmptySet (cardinality 0)
*           s1 difference s2 = EmptySet difference s2 = EmptySet  (because emptySet difference anything is emptySet)
*           EmptySet trivially satisfies the representational invariance i.e it has no duplicates i.e it is a set
*       
* IH: let sk difference s2 gives a set i.e. the operation preserves the representational invariance i.e all elements in resultant list are unique
*     where sk is a set with cardinality k
* 
* IS: sk+1 is a set with cardinality k+1
*     sk+1 = p::sk //in the above function p is the first element and thus sk contains remaining k elements none of which is p
*     for sk+1 two cases arise:
*       s' = sk difference s2 which preserves representational invariance (using induction hypothesis)
*       Case 1: p is not in s2, then above code gives p::(s')
*               where p cannot be in s' because it was not in sk
*               therefore p::s' also preserves representational invariance
*       Case 2: p is not in s2, then above code gives s' which is a set using IH
*             
* Hence the difference operation preserves the representational invariance of the inputs i.e the difference contains no duplicates
*)
print_string "a_set - b_set : "; print (difference a_set b_set);;
print_string "b_set - a_set : "; print (difference b_set a_set);;
print_string "\n";;



(**** 7. Cartesian Product ****)
let pair x y = (x,y);;
let rec product s1 s2 = match s1 with
                        [] -> []
                        | x::xs -> (List.map (fun y -> (x,y)) s2) @ (product xs s2)
                        ;;

(*      Proof of Preservance of Representational Invariance
*
* Given that the inputs s1 and s2 satisfy the representational invariance i.e. they have no duplicates (they are sets)
* Induction on Cardinality of s1
* BaseCase: s1 is EmptySet (cardinality 0)
*           s1 X s2 = EmptySet X s2 = EmptySet  (because emptySet X anything is emptySet)
*           EmptySet trivially satisfies the representational invariance i.e it has no duplicates i.e it is a set
*       
* IH: let sk X s2 gives a set i.e. the operation preserves the representational invariance i.e all pairs in resultant list are unique
*     where sk is a set with cardinality k
* 
* IS: sk+1 is a set with cardinality k+1
*     sk+1 = p::sk //in the above function p is the first element and thus sk contains remaining k elements none of which is p
*     s' = sk X s2 which preserves representational invariance (using induction hypothesis)
*     above method returns (p,Xi) appended to s' (for each Xi belonging to s2)
*     now since p is not in sk therefore the pair (p,Xi) cant belong to s' = sk X s2
*     hence also the (p,Xi)@(s') will contain no duplicate pairs
*       
* Hence the cartesian product operation preserves the representational invariance of the inputs i.e the cartesian product contains no duplicates
* i.e it is a set
*)

let rec print_prod s = match s with 
                       [] -> print_string "\n"
                       |(x,y)::xs -> Printf.printf "(%d,%d) " x y ; print_prod xs
                       ;;

print_string "a_set x b_set : " ; print_prod (product a_set b_set);;
print_string "\n";;



(**** 8. Power ****)
let rec powerset s = match s with
                     [] -> [emptySet]
                     |x::xs -> let ps = powerset xs in ps @ (List.map (fun y -> x::y) ps) 
                     ;;

(*      Proof of Preservance of Representational Invariance
*
* Given that the input s satisfy the representational invariance i.e. it has no duplicates (it is a set)
* Induction on Cardinality of s
* BaseCase: s is EmptySet 
            powerset of EmptySet contains one element that is EmptySet 
            therefore it satisfies the representational invariance i.e it has no duplicates i.e it is a set
*       
* IH: let powerset of sk is a set i.e the it preserves the representational invariance i.e all subsets in powerset of sk are unique
*     where sk is a set with cardinality k
* 
* IS: sk+1 is a set with cardinality k+1
*     sk+1 = p::sk //in the above function p is the first element and thus sk contains remaining k elements none of which is p
*     ps is powerset of sk //which is a set using IH
*     above method returns A appended to ps (where A is p::y for all y belonging to ps)
*      
*     now ps was a set (using IH) that is every element/subsets in ps was unique, therefore sets created by appending 
*     p to each of those subsets will also be unique. therefore A is also a set 
*
*     hence A @ ps is also a set because A contains unique elements, ps contains a unique elements and every element in A is formed by appendig
*     p to corresponding element in ps therefore no common element amongst A and ps too. 
*
*       
* Hence the powerset of S preserves the representational invariance of the inputs i.e the powerset contains no duplicates i,e it is a set
*)

let rec print_powerset s = match s with
                           [] -> print_string "\n"
                           | x::xs -> if x = emptySet then print_string "emptySet\n" else print x ; print_powerset xs
                           ;;

print_string "powerset of a_set : \n" ; print_powerset (powerset a_set);;

(*9* Subset *)
let rec subset s1 s2 = match s1 with
                       [] -> true
                       |x::xs -> if member x s2 then subset xs s2 else false
                       ;;

if subset [1;2] a_set then print_string "[1;2] is a subset of a_set\n" else print_string "[1;2] is not a subset of a_set\n";;
if subset [1;2] b_set then print_string "[1;2] is a subset of b_set\n" else print_string "[1;2] is not a subset of b_set\n\n";;


(*10* Equalset *)
let rec equalset s1 s2 = if subset s1 s2 && subset s2 s1 then true else false ;;

let p_set = [1;2];;
let q_set = [2;1];;

print_string "p_set : " ; print p_set;;
print_string "q_set : " ; print q_set;;
if equalset p_set q_set then print_string "p_set and q_set are equal\n" else print_string "p_set and q_set are not equal\n";;
if equalset p_set a_set then print_string "p_set and a_set are equal\n" else print_string "p_set and a_set are not equal\n\n";;


(********************************************************************************************************************************************)
(********************************************************************************************************************************************)

(************************* Various Laws **************************)

print_string "\n\t\t Shwoing Various Laws to be true\n\n";;
let z = 7;;

(**** 1 ****)
(* member z emptySet = false *)
if member z emptySet then print_string "member z emptySet = true\n" else print_string "member z emptySet = false\n\n";;

(*    Proof
* emptyset containse no element so any z does not belongs to it therefroe member z emptyset reutrns false 
*)



(**** 2 ****)
(* cardinality emptySet = 0 *)
print_string "Cardinality of empty set is: "; print_int (cardinality emptySet);;
print_string "\n\n";;

(*    Proof
* emptySet contains no elements therefore the number of elements and hence the cardinality of emptySet is zero 
*)



(**** 3 ****)
(* member x s1 implies member x (union s1 s2)*)
let s1 = [1;2;3];;
let s2 = [4;5;6];;
print_string "s1 : ";print s1;;
print_string "s2 : ";print s2;;
if member 1 (union s1 s2) then print_string "1 belongs to s1 U s2\n";;
if member 2 (union s1 s2) then print_string "2 belongs to s1 U s2\n";;
if member 3 (union s1 s2) then print_string "3 belongs to s1 U s2\n\n";;

(*    Proof
* Let x is a member of s1 and x is not a member of s1 U s2
* 
* From the definition of union funtcion, s1 U s2 contains all elements that are either in s1 or in s2 or both
* therefroe if x is not in s1 U s2 then it must not be in s1 as well as not in s2
* but we assumed that x is a member of s1 . Contradiction 
*
* Hence member x s1 implies member x (union s1 s2)
*)



(**** 4 ****)
(* member x (intersection s1 s2) implies member x s1*)
let s1 = [1;2;3];;
let s2 = [2;4];;
if member 2 (intersection s1 s2) then print_string "2 belongs to s1 intersection s2\n";;
if member 2 s1 then print_string "2 belongs to s1\n\n";;

(*    Proof
* Let x is a memeber of s1 intersection s2 but x is not a member of s1
*
* From the definition of intersection function, s1 intersection s2 contains all elements that are both in s1 as well as s2
* since x is a member of s1 intersection s2 therefore it must be both in s1 and s2
* but we assumed that x is not a member of s1. Contradiction
* 
* Hence member x (intersection s1 s2) implies member x s1
*)



(**** 5 ****)
(* equalset (intersection s1 s2) (intersection s2 s1) *)
if equalset (intersection s1 s2) (intersection s2 s1) then print_string "s1 intersection s2 is equal to s2 intersection s1\n\n";;

(*    Proof  
* pIq = p intersection q 
* qIp = q intersection p
* let there exist a element say 'x' that belongs to pIq but does not belong to qIp
*
* from definition of intersection function since 'x' belongs pIq therefore x must be a member of both p and q
* again from the definition of intersection if 'x' belongs to q and 'x' belongs to p then 'x' must belong to qIp
* but we assumed that x does note belongs to qIp. Contradiction
* 
* therefore if x belongs to pIq then x belongs to qIp , that is pIq is a subset of qIp
* similarly if x belongs to qIp then x belongs to pIq , that is qIp is a subset of pIq
*
* therfore from the definition of method equalset , since pIq is a subset of qIp and qIp is a subset of pIq both are equal
* Hence p intersection q and q intersection p are equalsets or equalset (intersection s1 s2) (intersection s2 s1)
*)



(**** 6 ****)
(* cardinality (product s1 s2) = (cardinality s1) * (cardinality s2) *)
Printf.printf "Cardinality of s1 : %d\n" (cardinality s1);;
Printf.printf "Cardinality of s2 : %d\n" (cardinality s2);;
Printf.printf "Cardinality of s1 x s2 : %d = %d * %d\n\n" (cardinality (product s1 s2)) (cardinality s1) (cardinality s2);;

(* Proof *)
(*  
* cardinality s1 = p 
* cardinality s2 = q
* from the definition of product function 
* in product s1 s2 every element of s1 is mapped to every element of s2
* that is for every element of s1 q pairs are formed
* therefore for p element number of pairs in product s1 s2 = p*q
* therefore cardinality of product s1 s2 = p*q = (cardinalty s1) * (cardinality s2)
*)


(********************************************************************************************************************************************)
(********************************************************************************************************************************************)


(*********** Sets using characteristic functions *************)

print_string "\n\t\t Sets using Characteristic function\n\n";;
print_string "two_set is set of multiples of 2\n";;
print_string "three_set is set of multiples of 3\n\n";;

(* Set of multiples of 2 *)
let two_set x = if x mod 2 = 0 then true
                       else false

(* Set of multiples of 3 *)
let three_set x = if x mod 3 = 0 then true
                       else false

(**** 1. EmptySet ****)
let func_emptyset () = false ;;
(*
  Always returns false therefore nothing belongs to this set
  therefore it is a characteristic function of emptySet
*)



(**** 2. MemberSet ****)
let func_member x s = s x ;;
(*
* above method returns the characteristic function of set s itself
*)
if func_member 4 two_set then print_string "4 belongs to set two_set\n" else print_string "4 does not belongs to set two_set\n";;
if func_member 4 three_set then print_string "4 belongs to set three_set\n" else print_string "4 does not belongs to set three_set\n";;
if func_member 3 two_set then print_string "3 belongs to set two_set\n" else print_string "3 does not belongs to set two_set\n";;
if func_member 3 three_set then print_string "3 belongs to set three_set\n" else print_string "3 does not belongs to set three_set\n";;
print_string "\n";;



(**** 3. Union ****)
let func_union s1 s2 = fun x -> s1 x || s2 x ;;
(*
* s1 x is true iff x belongs to set s1
* s2 x is true iff x belongs to set s2
* above function returns a function f s.t 
* f x = s1 x || s2 x
* f x is true when either s1 is true or s2 is true or both are true
* therefore f x will be true iff x belongs to set1 or set2 or both
* that is f x will be true iff x belongs to (set1 U set2)
*
* Thus the above function returns the characteristic function of s1 U s2
*)

if func_member 4 (func_union two_set three_set) then print_string "4 belongs to set two_set U three_set\n" else print_string "4 does not belongs to set two_set U three_set\n";;
if func_member 3 (func_union two_set three_set) then print_string "3 belongs to set two_set U three_set\n" else print_string "3 does not belongs to set two_set U three_set\n";;
print_string "\n";;



(**** 4. difference ****)
let func_intersection s1 s2 = fun x -> s1 x && s2 x;;
(*
* s1 x is true iff x belongs to set s1
* s2 x is true iff x belongs to set s2
* above function returns a function f s.t 
* f x = s1 x && s2 x
* f x is true iff both s1 and s2 are true
* therefore f x will be true iff x belongs to both set1 and set2 
* that is f x will be true iff x belongs to (set1 intersection set2)
*
* Thus the above function returns the characteristic function of s1 intersection s2
*)
if func_member 4 (func_intersection two_set three_set) then print_string "4 belongs to set two_set intersection three_set\n" else print_string "4 does not belongs to set two_set intersection three_set\n";;
if func_member 3 (func_intersection two_set three_set) then print_string "3 belongs to set two_set intersection three_set\n" else print_string "3 does not belongs to set two_set intersection three_set\n";;
if func_member 6 (func_intersection two_set three_set) then print_string "6 belongs to set two_set intersection three_set\n" else print_string "6 does not belongs to set two_set intersection three_set\n";;
print_string "\n";;



(**** 5. Difference ****)
let func_difference s1 s2 = fun x -> s1 x && (not (s2 x));;
(*
* s1 x is true iff x belongs to set s1
* s2 x is true iff x belongs to set s2
* above function returns a function f s.t 
* f x = s1 x && not(s2 x)
* f x is true iff s1 is true and s2 is false
* therefore f x will be true iff x belongs to set1 and x does not belongs to set2 
* that is f x will be true iff x belongs to (set1 - set2)
*
* Thus the above function returns the characteristic function of s1 - s2
*)
if func_member 4 (func_difference two_set three_set) then print_string "4 belongs to set two_set - three_set\n" else print_string "4 does not belongs to set two_set - three_set\n";;
if func_member 6 (func_difference two_set three_set) then print_string "6 belongs to set two_set - three_set\n" else print_string "6 does not belongs to set two_set - three_set\n";;
if func_member 6 (func_difference three_set two_set) then print_string "6 belongs to set three_set - two_set\n" else print_string "6 does not belongs to set three_set - two_set\n";;
print_string "\n";;



(**** 6. Cartesian Product ****)
let func_product s1 s2 = fun (x,y) -> s1 x && s2 y;;
(*
* s1 x is true iff x belongs to set s1
* s2 x is true iff x belongs to set s2
* above function returns a function f s.t 
* f (x,y) = s1 x && s2 y
* f (x,y) is true iff s1 x is true and s2 y is false
* therefore f(x,y) will be true iff x belongs to set1 and y belongs to set2 
* that is f(x,y) will be true iff (x,y) belongs to set1 X set2
*
* Thus the above function returns the characteristic function of s1 X s2
*)
if func_member (82,81) (func_product two_set three_set) then print_string "(82,81) belongs to set two_set x three_set\n" else print_string "(82,81) does not belongs to set two_set x three_set\n";;
if func_member (25,30) (func_product two_set three_set) then print_string "(25,30) belongs to set two_set x three_set\n" else print_string "(25,30) does not belongs to set two_set x three_set\n";;
if func_member (4,63) (func_product two_set three_set) then print_string "(4,63) belongs to set two_set x three_set\n" else print_string "(4,63) does not belongs to set two_set x three_set\n";;
if func_member (30,30) (func_product two_set three_set) then print_string "(30,30) belongs to set two_set x three_set\n" else print_string "(30,30) does not belongs to set two_set x three_set\n";;

print_string "\n";;