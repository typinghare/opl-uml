open Ast
open Helper

exception TypeError
exception UnificationError
exception UnimplementedError
exception ImpossibleBranchError

(* Set.choose: retrieve an arbitrary element from a set *)
(* Set.choose: remove an element from a set. `Set.choose 2 my_set` *)

(* [unify c0] solves [c0] (if possible), yielding a substitution.
   Raise UnificationError if constraints cannot be unified. *)
let rec unify(c0: constr): subst =
    if Constr.is_empty c0 then VarMap.empty else          (* Return an empty map if the constraint set is empty *)
    let (t1, t2): typ * typ = Constr.choose c0 in         (* Arbitrarily pick a constraint from the set *)
    let c: constr = Constr.remove (t1, t2) c0 in          (* c is the remaining constraint set *)
    if typ_eq t1 t2 then unify(c) else                    (* If t1 = t2 then return unify(c) *)
    match (t1, t2) with
    | (TVar(x), _) when not (VarSet.mem x (ftvs t2) ) ->  (* (1) t1 = X and X is not a free variable of t2 *)
        VarMap.add x t2 (unify(subst_constr c x t2))      (* Return unify(σ(C))∘σ, where σ = [X -> t2] *)
    | (_, TVar(x)) when not (VarSet.mem x (ftvs t1)) ->   (* (2) t2 = X and X is not a free variable of t1 *)
        VarMap.add x t1 (unify(subst_constr c x t1))      (* Return unify(σ(C))∘σ, where σ = [X -> t1] *)
    | (TArrow(t1, t2), TArrow(t1', t2')) ->               (* (3) t1 and t2 are function types *)
        unify(Constr.add (t2, t2') (Constr.add (t1, t1') c))
    | (TPair(t1, t2), TPair(t1', t2')) ->                 (* (4) t1 and t2 are tuple types *)
        unify(Constr.add (t2, t2') (Constr.add (t1, t1') c))
    | _ -> raise UnificationError

(* [check g e0] checks type [e0] in the context [g] generating a type and a set of constraints.
Raise TypeError if constraints cannot be generated. *)
let rec check (g: context) (e0: exp) : typ * constr =
    match e0 with
    (* For reference, I am providing the constraint generation for letrec. *)
    | Letrec (f, x, e1, e2) ->
      let t' = next_tvar () in                        (* τ *)
      let t'' = next_tvar () in                       (* τ1 *)
      let g' = VarMap.add f (TArrow (t', t'')) g in   (* f : (τ -> τ1) *)
      let g'' = VarMap.add x t' g' in                 (* x : τ *)
      let (t1, c1) = check g'' e1 in                  (* e1: τ1 ▷ C1 *)
      let (t2, c2) = check g' e2 in                   (* e2: τ2 ▷ C2 *)
      let c = Constr.union c1 c2 in
      let c' = Constr.add (t1, t'') c in              (* c' = C1 ∪ C2 ∪ (τ1 ≡ t'') *)
      (t2, c')                                        (* τ2 ▷ C' *)
    | Var(x) ->
        (try ((VarMap.find x g), Constr.empty)
        with Not_found -> raise TypeError)
    | App(e1, e2) ->
        let t' = next_tvar () in                      (* t' := X *)
        let (t1, c1) = check g e1 in
        let (t2, c2) = check g e2 in
        let c = Constr.union c1 c2 in
        let c' = Constr.add (t1, TArrow(t2, t')) c in (* C' = C1 ∪ C2 ∪ (τ1 ≡ τ2 -> X) *)
        (t', c')                                      (* X ▷ C' *)
    | Lam(x, e) ->
        let t1 = next_tvar () in
        let g' = VarMap.add x t1 g in
        let (t2, c) = check g' e in
        (TArrow(t1, t2), c)
    | Let(x, e1, e2) ->
        let (t1, c1) = check g e1 in
        let g' = VarMap.add x t1 g in
        let (t2, c2) = check g' e2 in
        (t2, Constr.union c1 c2)
    | Int(n) ->
        (TInt, Constr.empty)
    | Plus(e1, e2) | Times(e1, e2) | Minus(e1, e2) ->
        let (t1, c1) = check g e1 in
        let (t2, c2) = check g e2 in
        let c = Constr.add (t1, TInt) (Constr.add (t2, TInt) (Constr.union c1 c2)) in
        (TInt, c)
    | Pair(e1, e2) ->
        let (t1, c1) = check g e1 in
        let (t2, c2) = check g e2 in
        (TPair(t1, t2), Constr.union c1 c2)
    | Fst(e) ->
        let t1' = next_tvar () in
        let t2' = next_tvar () in
        let (t, c) = check g e in
        let c' = Constr.add (t, TPair(t1', t2')) c in
        (t1', c')
    | Snd(e) ->
        let t1' = next_tvar () in
        let t2' = next_tvar () in
        let (t, c) = check g e in
        let c' = Constr.add (t, TPair(t1', t2')) c in
        (t2', c')
    | True | False -> (TBool, Constr.empty)
    | Eq(e1, e2) ->
        let (t1, c1) = check g e1 in
        let (t2, c2) = check g e2 in
        let c = Constr.union c1 c2 in
        let c' = Constr.add (t1, TInt) c in
        let c'' = Constr.add (t2, TInt) c' in
        (TBool, c'')
    | If(e1, e2, e3) ->
        let (t1, c1) = check g e1 in
        let (t2, c2) = check g e2 in
        let (t3, c3) = check g e3 in
        let c = Constr.union (Constr.union c1 c2) c3 in
        let c' = Constr.add (t1, TBool) c in
        let c'' = Constr.add (t2, t3) c' in
        (t3, c'')