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
(* let rec eval' t = 
    match t with
    | TmTrue(_) -> t  (* true ⇓ true *)
    | TmFalse(_) -> t  (* false ⇓ false *)
    | TmIf(fi, t1, t2, t3) ->
        (* Perform a large step on t1 to determine which branch to go *)
        let t1' = eval t1 in
        begin
            match t1' with
            | TmTrue(_) -> eval t2  (* t1 ⇓ true *)
            | TmFalse(_) -> eval t3  (* t1 ⇓ false *)
            | _ -> t
        end
    | TmZero(_) -> t  (* 0 ⇓ 0 *)
    | TmSucc(fi, t1) -> 
        let nv1 = eval t1 in   (* Premise: t1 ⇓ nv1 *)
        eval1 (TmSucc(fi, nv1))  (* succ nv1 can be evaluated in one step *)
    | TmPred(fi, t1) ->
        let nv1 = eval t1 in  (* Premise: t1 ⇓ nv1 *)
        eval1 (TmPred(fi, nv1))  (* pred nv1 can be evaluated in one step *)
    | TmIsZero(fi, t1) -> 
        let t1' = eval t1 in 
        begin
            match t1' with
            | TmZero(_) -> TmTrue(fi)
            | TmSucc(_) -> TmFalse(fi)
            | _ -> t1
        end *)

let rec eval' t = 
    match t with
    | TmTrue(_) -> t                    (* true ⇓ true *)
    | TmFalse(_) -> t                   (* false ⇓ false *)
    | TmZero(_) -> t                    (* 0 ⇓ 0 *)
    | TmIf(fi, t1, t2, t3) ->
        let t1' = eval' t1 in           (* Perform a large step on t1 to determine which branch to go *)
        begin
            match t1' with
            | TmTrue(_) -> eval' t2     (* t1 ⇓ true *)
            | TmFalse(_) -> eval' t3    (* t2 ⇓ true *)
            | _ -> t                    (* t cannot be evaluated because t1 is not evaluated to a boolean *)
        end
    | TmSucc(fi, t1) ->
        let t1' = eval' t1 in           (* Premise: t1 ⇓ t1' *)
        begin
            match t1' with
            | TmZero(_) -> TmSucc(fi, t1')
            | TmPred(_, nv1) -> nv1     (* t1' is TmPred(_, nv1) *)
            | TmSucc(_, _) -> TmSucc(fi, t1')   
            | _ -> t
        end
    | TmPred(fi, t1) ->
        let t1' = eval' t1 in           (* Premise: t1 ⇓ t1' *)
        begin
            match t1' with
            | TmZero(_) -> t1'          (* B-PredZero *)
            | TmPred(_, _) -> TmPred(fi, t1')
            | TmSucc(_, nv1) -> nv1     (* t1' is TmSucc(_, nv1) *)
            | _ -> t
        end
    | TmIsZero(fi, t1) -> 
        let t1' = eval' t1 in           (* Evalue t1; it's evaluated if it is a number *)
        begin
            match t1' with
            | TmZero(_) -> TmTrue(fi)   (* Return true if t1' is 0 *)
            | TmSucc(_) -> TmFalse(fi)  (* Return false if t1' greater than 1 *)
            | _ -> t                    (* t cannot be evaluated *)
        end