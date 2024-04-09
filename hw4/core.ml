open Format
open Syntax
open Support.Error
open Support.Pervasive

(*
  Name: Zhuojian Chen (James)
  Section: 202
  Collaborators: Rohan Mallu; Anson Cheang
*)

(* ------------------------   EVALUATION  ------------------------ *)

(* Checks if a term is a value. *)
let rec isval ctx (t: term) =
  match t with
  | TmAbs(_, _, _) -> true
  | TmLoc(_, _) -> true
  | _ -> false

type store = term list  
let emptystore = []

(* Appends a term to a store, and return a new store. *)
let extendstore (store: store) (v: term) : int * store = (List.length store, List.append store [v])

(* Looks up a  *)
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

let rec eval1 (ctx: context) (store: store) (t: term) : term * store  =
  match t with
  | TmApp(fi, TmAbs(_, _, e), t2) when (isval ctx t2) ->    (* [App] *)
    ((termSubstTop t2 e), store)                            (* Beta reduction *)
  | TmApp(fi, abs, e1) when (isval ctx abs) ->              (* [RApp] *)
    let e1', store' = eval1 ctx store e1 in                 (* Evaluate e1 *)
    (TmApp(fi, abs, e1'), store')                           (* Return <abs e1', σ'> *)
  | TmApp(fi, e0, e1) ->                                    (* [LApp] *)
    let e0', store' = eval1 ctx store e0 in                 (* Evaluate e0 *)
    (TmApp(fi, e0', e1), store')                            (* Return <e0' e1, σ'> *)
  | TmRef(fi, v) when (isval ctx v) ->                      (* [Ref] *)
    let l, store' = extendstore store v in                  (* Insert the value into store *)
    (TmLoc(fi, l), store')                                  (* Return <l, σ'> *)
  | TmRef(fi, e) ->                                         (* [RefSimp] *) 
    let e', store' = eval1 ctx store e in                   (* Evaluate e *)
    (TmRef(fi, e'), store')                                 (* Return <ref e', σ'> *)
  | TmDeref(_, TmLoc(_, l)) ->                              (* [Deref] *)
    ((lookuploc store l), store)                            (* Return <v, σ'> *)
  | TmDeref(fi, e) ->                                       (* [DerefSimp] *)
    let e', store' = eval1 ctx store e in                   (* Evaluate e *)
    (TmDeref(fi, e'), store')                               (* Return <!e', σ'> *)
  | TmAssign(fi, TmLoc(_, l), v) when (isval ctx v) ->      (* [Assign] *)
    let store' = (updatestore store l v) in                 (* Update store *)
    (v, store')                                             (* Return <v, σ'> *)
  | TmAssign(fi, l, e1) when (isval ctx l)->                (* [RAssign] *)
    let e1', store' = eval1 ctx store e1 in                 (* Evaluate e1 *)
    (TmAssign(fi, l, e1'), store')                          (* Return <l := e1', σ'> *)
  | TmAssign(fi, e0, e1) ->                                 (* [LAssign] *)
    let e0', store' = eval1 ctx store e0 in                 (* Evaluate e0 *)
    (TmAssign(fi, e0', e1), store')                         (* Return <e0' := e1, σ'> *)
  | _ -> raise NoRuleApplies                                (* Var, Abs, and Loc are axioms *)

let rec eval ctx store t =
  try let t', store' = eval1 ctx store t
      in eval ctx store' t'
  with NoRuleApplies -> t, store
