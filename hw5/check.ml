open Ast
open Helper

exception TypeError 
exception UnificationError
exception UnimplementedError

(* [unify c0] solves [c0] (if possible), yielding a substitution. Raise UnificationError if constraints cannot be unified. *)
let rec unify (c0:constr) : subst = 
  (* FILL IN HERE FOR QUESTION 4 *)
  raise UnimplementedError

(* [check g e0] typechecks [e0] in the context [g] generating a type and a set of constraints. Raise TypeError if constraints cannot be generated. *)
let rec check (g:context) (e0:exp) : typ * constr = 
  match e0 with
  (* For reference, I am providing the constraint generation for letrec.
   *)
    | Letrec (f,x,e1,e2) ->
      let t' = next_tvar () in
      let t'' = next_tvar () in
      let g' = VarMap.add f (TArrow (t',t'')) g in
      let g'' = VarMap.add x t' g' in
      let (t1,c1) = check g'' e1 in
      let (t2,c2) = check g' e2 in
      let c = Constr.union c1 c2 in
      let c' = Constr.add (t1, t'') c in
      (t2, c')
    (* FILL IN HERE THE REMAINING CASES of e0 FOR QUESTION 4 *)
    | _ -> raise UnimplementedError
