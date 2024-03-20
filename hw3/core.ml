open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_, t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t -> true
  | _ -> false

(* small step semantics *)
let rec eval1 t = match t with
    TmIf(_,TmTrue(_),t2,t3) -> t2
  | TmIf(_,TmFalse(_),t2,t3) -> t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1) ->
      let t1' = eval1 t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')
  | _ -> 
      raise NoRuleApplies

(* multi-step; applies small-step evaluation to it repeatedly until no further reduction is possible *)
let rec eval t =
  try let t' = eval1 t in eval t'
  with NoRuleApplies -> t

 (* large step semantics *)
let rec eval' t = match t with
      TmTrue(_) -> t
    | TmFalse(_) -> t
    | TmIf(fi, t1, t2, t3) ->
        let t1' = eval t1 in
        begin
            match t1' with
            TmTrue(_) -> eval t2
            | TmFalse(_) -> eval t3
            | _ -> t
        end
    | TmZero(_) -> t
    | TmSucc(fi, t1) -> 
        let t1' = eval t1 in
        eval (TmSucc(fi, t1'))
    | TmPred(fi, t1) -> 
        let t1' = eval t1 in
        eval (TmPred(fi, t1'))
    | TmIsZero(fi, t1) -> 
        let t1' = eval t1 in
        eval (TmIsZero(fi, t1'))
