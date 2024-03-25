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
let rec eval1 (ctx: context) (store: store) (t: term) : term * store  =
  match t with
  | TmVar(_, _, _) -> 
    (* Var *)
    (t, store)  (* What should we do here? *)
  | TmAbs(_, _, _) -> 
    (* Abs *)
    (t, store)
  | TmLoc(_, _) ->
    (* Loc *)
    (t, store)
  | TmApp(fi, t1, t2) when (isval ctx t1) ->
    begin 
      match t1 with
      | TmAbs(_, _, e) when (isval ctx t2) ->
        (* App *)
        ((termSubstTop t2 e), store)   (* Beta reduction *)
      | TmAbs(_, _, _) ->
        (* RApp *)
        let t2', store' = eval1 ctx store t2 in
        (TmApp(fi, t1, t2'), store')
      | _ -> raise NoRuleApplies
    end
  | TmApp(fi, t1, t2) ->
    (* LApp *)
    let t1', store' = eval1 ctx store t1 in
    (TmApp(fi, t1', t2), store')
  | TmRef(fi, t1) when (isval ctx t1) ->
    (* Ref *)
    let loc, store' = extendstore store t1 in
    (TmLoc(fi, loc), store')
  | TmRef(fi, t1) ->
    (* RefSimp *) 
    let t1', store' = eval1 ctx store t1 in
    (TmRef(fi, t1'), store')
  | TmDeref(_, TmLoc(_, l)) ->
    (* Deref *)
    ((lookuploc store l), store)
  | TmDeref(fi, t1) ->
    (* DerefSimp *)
    let t1', store' = eval1 ctx store t1 in
    (TmDeref(fi, t1'), store')
  | TmAssign(fi, TmLoc(_, l), t2) when (isval ctx t2) ->
    (* Assign *)
    let store' = (updatestore store l t2) in
    (t2, store')
  | TmAssign(fi, t1, t2) when (isval ctx t1) ->
    (* RAssign *)
    let t2', store' = eval1 ctx store t2 in
    (TmAssign(fi, t1, t2'), store')
  | TmAssign(fi, t1, t2) ->
    (* LAssign *)
    let t1', store' = eval1 ctx store t1 in
    (TmAssign(fi, t1', t2), store')

let rec eval ctx store t =
  try let t', store' = eval1 ctx store t
      in eval ctx store' t'
  with NoRuleApplies -> t, store
