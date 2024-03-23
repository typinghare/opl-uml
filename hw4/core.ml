open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

(* Checks if a term is a value. *)
let rec isval ctx (t: term) =
  match t with
  | TmAbs(_, _, _) -> true
  | TmLoc(_, _) -> true
  | _ -> false

type store = term list  
let emptystore = []
let extendstore store v = (List.length store, List.append store [v])
let lookuploc store l = List.nth store l

(* Updates the store by storing a new term at a given location. *)
let updatestore (store: store) (n: int) (v: term) =
  let rec f s = match s with 
      (0, v'::rest) -> v::rest
    | (n, v'::rest) -> v' :: (f (n-1,rest))
    | _ -> error dummyinfo "updatestore: bad index"
  in
  
  f (n, store)
let shiftstore i store = List.map (fun t -> termShift i t) store 


exception NoRuleApplies

(* Implement small-step semantics for Lambda Calculus extended with References. *)
(* Refer to Lecture #15 for grammar and small-step semantics. *)
(* The grammar is shown on slide# 6/36). *)
(* The small step semantics are shown on slides 8/36 and 9/36. *)
(* HINT: To substitute v in t for all top variables (i.e., variables of index 0), *)
(*               use function `termSubstTop v t`  defined in syntax.ml. *)
let rec eval1 ctx store t =
  match t with
  | TmVar(_, _, _) -> (t, store)
  | TmAbs(_, _, _) -> (t, store)
  | TmApp(fi, t1, t2) ->
    if (isval ctx t1) then
      begin 
        match t1 with
        | TmAbs(_, x, e) -> (e, store) (* TODO: beta reduction *)
        | _ -> raise NoRuleApplies
      end
    else 
      let t1', store' = eval1 ctx store t1 in
      (TmApp(fi, t1', t2), store')
  | TmLoc(_, _) -> (t, store)
  | TmRef(fi, t1) -> 
    if (isval ctx t1) then
      let loc, store' = extendstore store t1 in
      (TmLoc(fi, loc), store')
    else
      let t1', store' = eval1 ctx store t1 in
      (TmRef(fi, t1'), store')
  | TmDeref(fi, t1) -> 
    begin 
      match t1 with
      | TmLoc(_, l) -> (lookuploc store l, store)
      | _ ->
        let t1', store' = eval1 ctx store t1 in
        (TmDeref(fi, t1'), store')
    end
  | TmAssign(_, _, _) -> (t, store)

let rec eval ctx store t =
  try let t', store' = eval1 ctx store t
      in eval ctx store' t'
  with NoRuleApplies -> raise NoRuleApplies
