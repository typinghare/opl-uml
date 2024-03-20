From Coq Require Import ZArith.ZArith.
From Coq Require Import Strings.String.
Open Scope string_scope.

Definition Var := string.
Definition Var_eq := String.eqb.
Definition Int := Z.
Definition Int_add := Z.add.
Definition Int_mul := Z.mul.
Definition Int_mod := Zmod.
Definition Int_fact := fun n => if (n <? 1)%Z then Zpos 1
                                else (Z.of_nat (fact (Z.to_nat n))).

Inductive Exp : Type :=
| EVar : Var -> Exp
| EInt : Int -> Exp
| EAdd : Exp -> Exp -> Exp
| EMul : Exp -> Exp -> Exp
| EAsg : Var -> Exp -> Exp -> Exp

(* ----- New constructors ----- *)

| EFact : Exp -> Exp
| EMod : Exp -> Exp -> Exp.

Definition Store := Var -> Int.

Definition Config := (Exp * Store)%type.
Definition FinalConfig := (Int * Store)%type.

Definition StoreUpdate (s : Store) (x : Var) (n : Int) : Store :=
  fun (y : Var) => if (Var_eq x y) then n else (s y).

Implicit Types (n m : Int) (x : Var) (e f g : Exp) (s t u : Store).
Reserved Notation "c1 '-->' c2" (at level 40).

Inductive lstep : Config -> FinalConfig -> Prop :=
| LSInt : forall n s,
            lstep (EInt n, s) (n, s)
| LSVar : forall x s,
            lstep (EVar x, s) (s x, s)
| LSAdd : forall n m e f s t u,
            lstep (e, s) (n, t) ->
            lstep (f, t) (m, u) ->
              lstep (EAdd e f, s) (Int_add n m, u)
| LSMul : forall n m e f s t u,
            lstep (e, s) (n, t) ->
            lstep (f, t) (m, u) ->
              lstep (EMul e f, s) (Int_mul n m, u)
| LSAsg : forall n m x e f s t u,
             lstep (e, s) (n, t) ->
             lstep (f, StoreUpdate t x n) (m, u) ->
               lstep (EAsg x e f, s) (m, u)
| LSFact : forall n e s t,
             lstep (e, s) (n, t) ->
               lstep (EFact e, s) (Int_fact n, s)
| LSMod : forall e f m n s t u,
            (n > 0)%Z ->
            lstep (e, s) (m, t) ->
            lstep (f, t) (n, u) ->
              lstep (EMod e f, s) (Int_mod m n, u).

(* ----- PROBLEM 1 (a, b) -----

   Extend `sstep` below to handle factorial and modulo `Exp`s,
   agreeing with the large-step semantics above. *)

Inductive sstep : Config -> Config -> Prop :=
| SSVar : forall x s,
            (EVar x, s) --> (EInt (s x), s)
| SSAdd : forall n m s,
            (EAdd (EInt n) (EInt m), s) --> (EInt (Int_add n m), s)
| SSMul : forall n m s,
            (EMul (EInt n) (EInt m), s) --> (EInt (Int_mul n m), s)
| SSAsg : forall n x e s,
             (EAsg x (EInt n) e, s) --> (e, StoreUpdate s x n)
| SSLAdd : forall e f g s t,
             (e, s) --> (f, t)  ->
               (EAdd e g, s) --> (EAdd f g, t)
| SSRAdd : forall n e f s t,
             (e, s) --> (f, t)  ->
               (EAdd (EInt n) e, s) --> (EAdd (EInt n) f, t)
| SSLMul : forall e f g s t,
             (e, s) --> (f, t)  ->
               (EMul e g, s) --> (EMul f g, t)
| SSRMul : forall n e f s t,
             (e, s) --> (f, t)  ->
               (EMul (EInt n) e, s) --> (EMul (EInt n) f, t)
| SSAsg1 : forall x e f g s t,
              (e, s) --> (f, t)  ->
                (EAsg x e g, s) --> (EAsg x f g, t)
(* Factorial small step constructor *)
| SSFact : forall e n s,
              (e, s) --> (EInt n, s) ->
                (EFact e, s) --> (EInt (Int_fact n), s)
| SSMod : forall e f m n s t u,
             (e, s) --> (EInt m, t) ->
             (f, t) --> (EInt n, u) ->
             (n > 0)%Z ->
               (EMod e f, s) --> (EInt (Int_mod m n), u)

where "c1 '-->' c2" := (sstep c1 c2).

Definition relation (X : Type) := X -> X -> Prop.

Inductive multi {X : Type} (R : relation X) : relation X :=
  | multi_refl : forall (x : X), multi R x x
  | multi_step : forall (x y z : X),
                    R x y ->
                    multi R y z ->
                    multi R x z.

Definition multisstep := (multi sstep).
Notation "t1 '-->*' t2" := (multisstep t1 t2) (at level 40).

(* ----- CHALLENGE -----

   Prove that your answer to 1(a,b) is correct. *)

(*
Theorem semantic_equivalence :
  forall e n s t, lstep (e, s) (n, t) <-> (e, s) -->* (EInt n, t).
Proof.
  (* your proof here *)
Admitted.
*)

(* ----- PROBLEM 1 (c) -----

   Formalize the example <(fact 3) mod (-7), s0> and prove something
   about its behavior in the large and small step semantics. *)

(* I don't know what I should prove here. *)