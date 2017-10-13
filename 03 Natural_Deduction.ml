(* Inert element a in set s *)
let insert a s = if List.mem a s then s else s @ a::[] ;; 

(**** 4. Union ****)
let rec union s1 s2 = match s1 with 
[] -> s2
| x::xs -> insert x (union xs s2)
;;


let member x s = if List.mem x s then true else false ;;

let rec intersection s1 s2 = match s1 with
[] -> []
| x::xs -> if member x s2 then x :: (intersection xs s2)
           else intersection xs s2
;;

let rec subset s1 s2 = match s1 with
[] -> true
|x::xs -> if member x s2 then subset xs s2 else false
;;

let rec equalset s1 s2 = if subset s1 s2 && subset s2 s1 then true else false ;;

let rec difference s1 s2 = match s1 with
[] -> []   
| x::xs -> if not (member x s2) then x :: (difference xs s2)
else  difference xs s2
;;

(***** Declaring  new data type for proposition *****)

type prop = P of string 
  | T 
  | F
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Implies of prop * prop;;



(***** Declaring  new data type for sequent/judgments *****)

type sequent = prop list * prop;;



(***** Declaring  new data type for prooftrees *****)

type prooftree  = Ass of sequent 
  | TI of sequent 
  | FE of sequent
  | ImpI of prooftree * sequent 
  | ImpE of prooftree * prooftree * sequent
  | AndI of prooftree * prooftree * sequent 
  | AndEleft of prooftree * sequent 
  | AndEright of prooftree * sequent
  | OrIleft of prooftree * sequent 
  | OrIright of prooftree * sequent 
  | OrE of prooftree * prooftree * prooftree * sequent
  | NotClass of  prooftree * sequent 
  | NotIntu of prooftree * sequent ;; 

(* 3.0 sequent at root of tree *)
let rec root_seq tree = match tree with
    Ass seq -> seq
  | TI seq -> seq
  | FE seq -> seq
  | ImpI (tree1, seq) -> seq
  | ImpE (tree1, tree2, seq) -> seq
  | AndI (tree1, tree2, seq) -> seq
  | AndEleft (tree1, seq) -> seq
  | AndEright (tree1, seq) -> seq
  | OrIleft (tree1, seq) -> seq
  | OrIright (tree1, seq) -> seq
  | OrE (tree1, tree2, tree3, seq) -> seq
  | NotClass (tree1, seq) -> seq
  | NotIntu (tree1, seq) -> seq ;;



(* 3.1 ht : prooftree -> int = <fun> *)
(* which returns the height of a prooftree *)

let rec ht tree = match tree with
    Ass seq -> 0
  | TI seq -> 0
  | FE seq -> 0
  | ImpI (tree1, seq) -> 1 + ht tree1
  | ImpE (tree1, tree2, seq) -> 1 + max (ht tree1) (ht tree2)
  | AndI (tree1, tree2, seq) -> 1 + max (ht tree1) (ht tree2)
  | AndEleft (tree1, seq) -> 1 + ht tree1
  | AndEright (tree1, seq) -> 1 + ht tree1
  | OrIleft (tree1, seq) -> 1 + ht tree1
  | OrIright (tree1, seq) -> 1 + ht tree1
  | OrE (tree1, tree2, tree3, seq) -> 1 + max (max (ht tree1) (ht tree2)) (ht tree3)
  | NotClass (tree1, seq) -> 1 + ht tree1
  | NotIntu (tree1, seq) -> 1 + ht tree1 ;;



(* 3.2 size : prooftree -> int = <fun> *)
(* which returns the number of nodes (rules used) in a prooftree *)

let rec size tree = match tree with
    Ass seq -> 1
  | TI seq -> 1
  | FE seq -> 1
  | ImpI (tree1, seq) -> 1 + size tree1
  | ImpE (tree1, tree2, seq) -> 1 + size tree1 + size tree2
  | AndI (tree1, tree2, seq) -> 1 + size tree1 + size tree2
  | AndEleft (tree1, seq) -> 1 + size tree1
  | AndEright (tree1, seq) -> 1 + size tree1
  | OrIleft (tree1, seq) -> 1 + size tree1
  | OrIright (tree1, seq) -> 1 + size tree1
  | OrE (tree1, tree2, tree3, seq) -> 1 + size tree1 + size tree2 + size tree3
  | NotClass (tree1, seq) -> 1 + size tree1
  | NotIntu (tree1, seq) -> 1 + size tree1 ;;



(* 3.3 wfprooftree : prooftree -> bool = <fun> *)
(* It checks that a given candidate proof tree is indeed a well-formed proof tree 
(i.e., the main formula is of the form expected by the rule, the side formulas are 
consistent with the main formula, and the extra formulas agree as specified in each rule). *)

let rec wfprooftree tree = match tree with
    Ass (g, p) -> List.mem p g  (* Assumption is well formed if p belongs to g*)

  | TI (g, p) -> p = T  (*True Introdunction, conclusion of sequent shoudl be T*)

  | FE (g, p) -> List.mem F g  (*False Introdunction,g must have F*)

  | ImpI (tree1, (g, p)) -> (match p with 
                                 Implies (p1, p2) -> let (gt, pt) = root_seq tree1 in 
                                                     (equalset gt (insert p1 g))    (*The gammas of the anticedent must be union of gamma and p1 of conseuqent*)
                                                     && (p2 = pt)   (*The conclusion fo the anticedent must be eqal to p2*)
                                                     && (wfprooftree tree1)   (*The prooftree of the anticedent should also be well formed*)
                                | _ -> false)   (* The conclusion of the consequent must be an implication*)

  | ImpE (tree1, tree2, (g, p)) -> let (gt1,pt1) = root_seq tree1 in 
                                          let (gt2, pt2) = root_seq tree2 in (
                                            match pt1 with 
                                              Implies(q1, q2) -> (q2 = p)   (*The conclusion of the consequent must be equal to q2*)
                                                                 && (q1 = pt2)    (*The conclusion of second anticedent must be equal to q1*) 
                                                                 && (equalset g gt1) && (equalset g gt2)    (*The hypothesis or gammas of all the antecedents and consequent must be equal*)
                                                                 && (wfprooftree tree1) && (wfprooftree tree2)    (*The prooftree of the anticedents should also be well formed*)
                                            | _ -> false    (* The conclusion of the first anticedent must be an implication*)
                                          )

  | AndI (tree1, tree2, (g, p)) -> (match p with 
                                        And (p1, p2) -> let (gt1, pt1) = root_seq tree1 in
                                                          let (gt2, pt2) = root_seq tree2 in
                                                              (p1 = pt1) && (p2 = pt2)    (*The concluions of the antecedents must be equal to p1, p2 respectively*)
                                                              && (equalset g gt1) && (equalset g gt2)    (*The hypothesis or gammas of all the antecedents and consequent must be equal*)
                                                              && (wfprooftree tree1) && (wfprooftree tree2)   (*The prooftree of the anticedent should also be well formed*)
                                        | _ -> false)   (* The conclusion of the consequent must be an And of two props*)

  | AndEleft (tree1, (g, p)) -> let (gt1, pt1) = root_seq tree1 in
                                    (match pt1 with 
                                      And (p1, p2) -> (p1 = p)    (*The conclusion of the consequent should be equal to p1*) 
                                                      && (equalset g gt1)   (*The hypothesis or gammas of the anticedent and consequent must be equal*)  
                                                      && (wfprooftree tree1)    (*The prooftree of the anticedent should also be well formed*)
                                    | _ -> false)   (* The conclusion of the anticedent must be an And of two props*)

  | AndEright (tree1, (g, p)) -> let (gt1, pt1) = root_seq tree1 in
                                    (match pt1 with 
                                      And (p1, p2) -> (p2 = p)    (*The conclusion of the consequent should be equal to p2*)
                                                      && (equalset g gt1)   (*The hypothesis or gammas of the anticedent and consequent must be equal*)    
                                                      && (wfprooftree tree1)    (*The prooftree of the anticedent should also be well formed*)
                                    | _ -> false)   (* The conclusion of the anticedent must be an And of two props*)

  | OrIleft (tree1, (g, p)) -> (match p with
                                Or (p1, p2) -> let (gt1, pt1) = root_seq tree1 in
                                                  (pt1 = p1)    (* The conclusion of the antecedent must must be equal to p1*)
                                                  && (equalset g gt1)   (*The hypothesis or gammas of the anticedent and consequent must be equal*)    
                                                  && (wfprooftree tree1)    (*The prooftree of the anticedent should also be well formed*)
                              | _ -> false)   (* The conclusion of the consequent must be an Or of two props*)

  | OrIright (tree1, (g, p)) -> (match p with
                                Or (p1, p2) -> let (gt1, pt1) = root_seq tree1 in
                                                  (pt1 = p2)    (*The conclusion of the antecedent mut be equal to p2*)
                                                  && (equalset g gt1)   (*The hypothesis or gammas of the anticedent and consequent must be equal*)    
                                                  && (wfprooftree tree1)    (*The prooftree of the anticedent should also be well formed*)
                              | _ -> false)   (* The conclusion of the consequent must be an Or of two props*)
  
  | OrE (tree1, tree2, tree3, (g, p)) -> let (gt1, pt1) = root_seq tree1 in
                                          let (gt2, pt2) = root_seq tree2 in
                                            let (gt3, pt3) = root_seq tree3 in
                                              (match pt1 with 
                                                Or (p1, p2) -> (pt2 == p) && (pt3 == p)   (*The conclusion of the the right two anticedents must be equal to the conclusion the consequent*)
                                                            && (equalset g gt1) && (equalset (insert p1 g) gt2) && (equalset (insert p2 g) gt3) 
                                                            && (wfprooftree tree1) && (wfprooftree tree2) && (wfprooftree tree3)
                                              | _ -> false)   (* The conclusion of the first anticedent must be an Or of two props*)

  | NotClass (tree1, (g, p)) -> let (gt1, pt1) = root_seq tree1 in
                                  (equalset (insert (Not p) g) gt1)
                                  && (pt1 = F)
                                  && wfprooftree tree1

  | NotIntu (tree1, (g, p)) -> let (gt1, pt1) = root_seq tree1 in
                                  equalset g gt1 
                                  && (pt1 = F)
                                  && wfprooftree tree1
  ;;



(* 3.4 pad : prooftree -> prop list -> prooftree = <fun> *)
(* that given a well-formed proof tree and a set of additional assumptions, 
creates a new well-formed proof tree with the set of additional assumptions added at each node. *)
let rec pad tree d = match tree with
  Ass (g, p) -> Ass (union g d, p)
| TI (g, p) -> TI (union g d, p)
| FE (g, p) -> FE (union g d, p)
| ImpI (tree1, (g, p)) -> ImpI (pad tree1 d, (union g d, p))
| ImpE (tree1, tree2, (g, p)) -> ImpE (pad tree1 d, pad tree2 d, (union g d, p))
| AndI (tree1, tree2, (g, p)) -> AndI (pad tree1 d, pad tree2 d, (union g d, p))
| AndEleft (tree1, (g, p)) -> AndEleft (pad tree1 d, (union g d, p))
| AndEright (tree1, (g, p)) -> AndEright (pad tree1 d, (union g d, p))
| OrIleft (tree1, (g, p)) -> OrIleft (pad tree1 d, (union g d, p))
| OrIright (tree1, (g, p)) -> OrIright (pad tree1 d, (union g d, p))
| OrE (tree1, tree2, tree3, (g, p)) -> OrE (pad tree1 d, pad tree2 d, pad tree3 d, (union g d, p))
| NotClass (tree1, (g, p)) -> NotClass (pad tree1 d, (union g d, p))
| NotIntu (tree1, (g, p)) -> NotIntu (pad tree1 d, (union g d, p)) 
;;


(* 3.5 pare : prooftree -> prooftrwee = <fun> *)
(* given awell-formed prooftree, returns a well-formed proof 
tree with minimal assumptions in each sequent *)
exception NotWellFormed;;

let rec pare tree = match tree with
  Ass (g, p) -> Ass ([p], p)
| TI (g, p) -> TI ([], T)
| FE (g, p) -> FE ([F], p)
| ImpI (tree1, (g,p)) -> (
                          match p with
                          Implies(p1,p2) -> let ptree1 = pare tree1 in
                                            ImpI(pad ptree1 [p1], (difference (fst (root_seq ptree1)) [p1], p))
                                            | _ -> raise NotWellFormed
                         )
| ImpE (tree1, tree2, (g,p)) -> let ptree1 = pare tree1 in
                                let ptree2 = pare tree2 in
                                let g1 = fst (root_seq ptree1) in
                                let g2 = fst (root_seq ptree2) in
                                  ImpE(pad ptree1 g2, pad ptree2 g1, (union g1 g2, p))

| AndI (tree1, tree2, (g,p)) -> let ptree1 = pare tree1 in
                                let ptree2 = pare tree2 in
                                let g1 = fst (root_seq ptree1) in
                                let g2 = fst (root_seq ptree2) in
                                  AndI(pad ptree1 g2, pad ptree2 g1, (union g1 g2, p))
| AndEleft (tree1, (g,p)) -> let ptree1 = pare tree1 in 
                             let g1 = fst (root_seq ptree1) in
                                AndEleft(ptree1, (g1, p))
| AndEright (tree1, (g,p)) -> let ptree1 = pare tree1 in 
                              let g1 = fst (root_seq ptree1) in
                                AndEright(ptree1, (g1, p))
| OrIleft (tree1, (g,p)) -> let ptree1 = pare tree1 in 
                            let g1 = fst (root_seq ptree1) in
                              OrIleft(ptree1, (g1, p))
| OrIright (tree1, (g,p)) -> let ptree1 = pare tree1 in 
                             let g1 = fst (root_seq ptree1) in
                               OrIright(ptree1, (g1, p))
| OrE (tree1, tree2, tree3, (g, p)) -> let ptree1 = pare tree1 in
                                       let ptree2 = pare tree2 in
                                       let ptree3 = pare tree2 in
                                       let g1 = fst (root_seq ptree1) in
                                       let g2 = fst (root_seq ptree2) in
                                       let g3 = fst (root_seq ptree3) in
                                          (match snd (root_seq ptree1) with
                                          Or(p1,p2) -> OrE(pad ptree1 (difference (union g2 g3) [p1;p2]), pad ptree2 (insert p1 (difference (union g1 g3) [p2])), pad ptree3 (insert p2 (difference (union g1 g2) [p1])), (difference (union g3 (union g1 g2))[p1;p2], p))
                                          | _ -> raise NotWellFormed
                                          )
| NotClass (tree1, (g, p)) -> let ptree1 = pare tree1 in 
                              let g1 = fst (root_seq ptree1) in
                                NotClass(pad ptree1 [Not(p)], (g1, p))                                          
| NotIntu (tree1, (g, p)) ->  let ptree1 = pare tree1 in 
                              let g1 = fst (root_seq ptree1) in
                                NotIntu(ptree1, (g1, p)) 
;; 


(* 3.6 graft : prooftree -> prooftree list -> prooftree = <fun> *)
let rec graft tree tree_list = match tree with 
  Ass seq -> List.find (fun tr -> snd (root_seq tr) = snd seq) tree_list   
| TI seq -> TI ((fst (root_seq (List.nth tree_list 0))), T)
| FE seq -> FE ((fst (root_seq (List.nth tree_list 0))), snd seq)
| ImpI (tree1, seq) -> let gtree1 = graft tree1 tree_list in ImpI(gtree1, (fst (root_seq gtree1),snd seq))
| ImpE (tree1, tree2, seq) -> let gtree1 = graft tree1 tree_list in ImpE( gtree1, (graft tree2 tree_list), (fst (root_seq gtree1),snd seq))
| AndI (tree1, tree2, seq) -> let gtree1 = graft tree1 tree_list in AndI( gtree1, (graft tree2 tree_list), (fst (root_seq gtree1),snd seq))
| AndEleft (tree1, seq) -> let gtree1 = graft tree1 tree_list in AndEleft(gtree1, (fst (root_seq gtree1),snd seq))
| AndEright (tree1, seq) -> let gtree1 = graft tree1 tree_list in AndEright(gtree1, (fst (root_seq gtree1),snd seq))
| OrIleft (tree1, seq) -> let gtree1 = graft tree1 tree_list in OrIleft(gtree1, (fst (root_seq gtree1),snd seq))
| OrIright (tree1, seq) -> let gtree1 = graft tree1 tree_list in OrIright(gtree1, (fst (root_seq gtree1),snd seq))
| OrE (tree1, tree2, tree3, seq) -> let gtree1 = graft tree1 tree_list in
                                    OrE(gtree1, (graft tree2 tree_list), (graft tree3 tree_list), 
                                           (fst (root_seq gtree1),snd seq)
                                          )
| NotClass (tree1, seq) -> let gtree1 = graft tree1 tree_list in NotClass(gtree1, (fst (root_seq gtree1),snd seq))
| NotIntu (tree1, seq) -> let gtree1 = graft tree1 tree_list in NotIntu(gtree1, (fst (root_seq gtree1),snd seq))
;;


(* 3.7 normalise : prooftree -> prooftree = <fun>  *)
(* removes all occurrences of r-pairs in a given well-formed prooftree *)
(* i.e.,where an introduction rule is followed only by an elimination rule of the main connective *)
let rec normalise tree = match tree with
AndEleft((AndI(tree1, tree2, seq)), seq2) -> tree1
| AndEright((AndI(tree1, tree2, seq)), seq2) -> tree2
| ImpE( (ImpI(tree1, seq1)), tree2, seq2) -> graft tree1 [tree2]
| OrE((OrIleft(tree1, seq1)), tree2, tree3, seq) -> graft tree2 [tree1]
| OrE((OrIright(tree1, seq1)), tree2, tree3, seq) -> graft tree3 [tree1]
| _ -> tree;;
