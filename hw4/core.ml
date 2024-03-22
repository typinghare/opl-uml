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
let rec eval1 ctx store t =       raise NoRuleApplies

let rec eval ctx store t =
  try let t',store' = eval1 ctx store t
      in eval ctx store' t'
  with NoRuleApplies -> raise NoRuleApplies
