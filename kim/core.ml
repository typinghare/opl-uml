(*

  Collaborators:
  =============


  
  Submit this file in GradeScope HW4: Implementing References
*)


open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

let rec isval ctx t = match t with
    TmAbs(_,_,_) -> true
  | TmLoc(_,_) -> true
  | _ -> false

type store = term list  
let emptystore = []
let extendstore store v = (List.length store, List.append store [v])
let lookuploc store l = List.nth store l
let updatestore store n v =
  let rec f s = match s with 
      (0, v'::rest) -> v::rest
    | (n, v'::rest) -> v' :: (f (n-1,rest))
    | _ -> error dummyinfo "updatestore: bad index"
  in
    f (n,store)
let shiftstore i store = List.map (fun t -> termShift i t) store 


exception NoRuleApplies

(* Implement small-step semantics for Lambda Calculus extended with References. *)
(*                Refer to Lecture #15 for grammar and small-step semantics. *)
(*                The grammar is shown on slide# 6/36). *)
(*                The small step semantics are shown on slides 8/36 and 9/36. *)

(* HINT: To substitute v in t for all top variables (i.e., variables of index 0), *)
(*               use function `termSubstTop v t`  defined in syntax.ml. *)
(**check if the input is value  *)
  let rec is_location t = match t with 
  | TmLoc(_,_) -> true
  | _ -> false
  

let rec eval1 ctx store t = match t with

 
  (* BETA-REDUCTION *)
  (* | TmApp(fi, t1, t2) ->
    begin match (t1, t2) with
    | (TmAbs(_, x, t12), _) when isval ctx t2 ->
        (* APP rule: If t1 is an abstraction and t2 is a value, perform beta reduction *)
        let t' = termSubstTop t2 t12 in
        (t', store)
    | (_, _) when not (isval ctx t1) ->
        (* LAPP rule: If t1 is not a value, evaluate t1 *)
        let t1', store' = eval1 ctx store t1 in
        (TmApp(fi, t1', t2), store')
    | (TmAbs(_, _, _), _) ->
        (* RAPP rule: If t1 is an abstraction (and thus a value) but t2 is not a value, evaluate t2 *)
        let t2', store' = eval1 ctx store t2 in
        (TmApp(fi, t1, t2'), store')
    | _ ->
        (* This case handles any other term configurations that are not specifically
           handled by the above patterns *)
        raise NoRuleApplies
    end *)
  (* BETA-REDUCTION (APP rule) *)
    
    
  (* | TmLoc(_,_) -> ctx store t *)
  | TmApp(fi, TmAbs(_, x, t12), v2) when isval ctx v2 ->
  print_string "TmApp 1 \n"; 
  let t' = termSubstTop v2 t12 in
  (t', store)
(* LAPP rule *)
| TmApp(fi, t1, t2) when not (isval ctx t1) ->
  print_string "TmApp 2 \n"; 
  let t1', store' = eval1 ctx store t1 in
  (TmApp(fi, t1', t2), store')
(* RAPP rule *)
| TmApp(fi, v1, t2) when isval ctx v1 && not (isval ctx t2) ->
  print_string "TmApp 3 \n"; 
  let t2', store' = eval1 ctx store t2 in
  (TmApp(fi, v1, t2'), store')
(* Catch-all for TmApp to ensure all cases are handled *)
| TmApp(_, _, _)
 -> 
  print_string "TmApp 4 \n"; 
  raise NoRuleApplies

  (* ALLOC *)
  | TmRef(fi, t)  ->
    print_string "Creating a new reference\n"; 
    if isval ctx t then
      begin
      print_string "inside if case \n";
      let (l, new_store) = extendstore store t in
      (TmLoc(dummyinfo, l), new_store)
      end
    else
      begin
        print_string "inside else case \n";
        let t', store' = eval1 ctx store t in
        eval1 ctx store' (TmRef(fi, t'))
      end 
    (* DEREF *)
  | TmDeref(fi, t) ->
    if isval ctx t then match t with
      | TmLoc(_, l) ->
        (lookuploc store l, store)
      | _ -> raise NoRuleApplies
    else 
      let t', store' = eval1 ctx store t in 
      eval1 ctx store' (TmDeref(fi, t'))
  (* ASSIGN *)
    | TmAssign(fi, e0, e1) ->
      begin match e0, e1 with
        | TmLoc(_, l), _ when isval ctx e1 ->
          if l < List.length store then
            let new_store = updatestore store l e1 in
            (TmLoc(fi, l), new_store)  (* The store is updated with the value at location l *)
          else raise NoRuleApplies  (* Location is out of bounds *)
        | _, _ when is_location e0 && not (isval ctx e1) ->
            let e1', store' = eval1 ctx store e1 in
            eval1 ctx store' (TmAssign(fi, e0, e1'))  (* RASSIGN: evaluate the right side *)
        | _, _ ->
            let e0', store' = eval1 ctx store e0 in
            eval1 ctx store' (TmAssign(fi, e0', e1))  (* LASSIGN: evaluate the left side *)
      end

    | _ -> 
      print_string "ending raise \n"; 
      raise NoRuleApplies ;; 
  
  let rec eval ctx store t =
    try let t',store' = eval1 ctx store t
        in eval ctx store' t'
    with NoRuleApplies -> raise NoRuleApplies