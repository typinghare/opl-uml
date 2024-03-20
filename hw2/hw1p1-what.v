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
| SSEFact : forall e f s s',
                (e, s) --> (f, s') ->
                (EFact e, s) --> (EFact f, s')
| SSVFact : forall s n,
                (EFact (EInt n), s) --> (EInt (Int_fact n), s)
| SSLMod : forall e f h s s',
                (e, s) --> (f, s') ->
                (EMod e h, s) --> (EMod f h, s')
| SSRMod : forall e f h s s',
                (e, s) --> (h, s') ->
                (EMod f e, s) --> (EMod f h, s')
| SSMod : forall s m n s,
                (n >0)%Z -> (EMod (EInt m) (EInt n), s) --> (EInt (Int_mod m n), s)
(* ----- Your constructors here ----- *)

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


(* ----- PROBLEM 1 (c) -----

   Formalize the example <(fact 3) mod (-7), s0> and prove something
   about its behavior in the large and small step semantics. *)

Check (EInt (-11)%Z).

(* compute whether lstep (3, 0) (3, 0) holds *)
Eval compute in (lstep ((EInt 3%Z), (fun _ => 0%Z)) (3%Z, (fun _ => 0%Z))).

(* compute whether lstep (fact 3, 0) (6, 0) holds *)
Eval compute in (lstep ((EFact (EInt 3%Z)), (fun _ => 0%Z)) (6%Z, (fun _ => 0%Z))).

(* compute whether lstep (fact 3, 0) (6, 0) holds *)
Eval compute in (lstep ((EFact (EInt 3%Z)), (fun _ => 0%Z)) (4%Z, (fun _ => 0%Z))).

(* Compute whether (fact 3, 0) --> (6, 0) holds *)
Eval compute in  ((EFact (EInt 3%Z)), (fun _ => 0%Z)) --> ((EInt 2%Z), (fun _ => 0%Z)).


Example somesmallstep: forall s, ((EMod (EFact (EInt 3%Z)) (EInt (-7)%Z)), s) --> ((EMod (EInt 6%Z) (EInt (-7)%Z)), s).
Proof.
  intros.
  simpl.
  constructor.
  constructor.
Qed.

Example somelargestep: forall m s s', lstep ((EMod (EFact (EInt 3%Z)) (EInt (-7)%Z)), s) (m, s') -> False.
Proof.
  intros.
  inversion H.
  inversion H7.
  rewrite H11 in H9.
  rewrite <- H11 in H4.
  discriminate H4.
Qed.

(* lstep (3+2, s) (5, s) *) 
Example trivial1: forall s, lstep ((EAdd (EInt 3%Z) (EInt 2%Z)), s) ((Int_add 3%Z 2%Z), s).
Proof.
  intros.
  econstructor 3.
  constructor.
  constructor.
Qed.

(* lstep (fact 3, s) (6, s) *) 
Example trivial2: forall s, lstep (EFact (EInt 3%Z), s) ((Int_fact 3%Z), s).
Proof.
  intros.
  econstructor 6.
  constructor.
Qed.

(* lstep (mod 3 2, s) (1, s) *) 
Example trivial3: forall s, lstep ((EMod (EInt 3%Z) (EInt 2%Z)), s) (1%Z, s).
Proof.
  intros.
  econstructor 7. (* this does not work *)
  - (* Prove (3 > 0) *)
    constructor.
    trivial.
  - (* Prove lstep ((EInt 3%Z), s) (3%Z, s) *)
    econstructor 1. (* Use LStep_Int *)
    constructor.
    trivial.
  - (* Prove lstep ((EInt 2%Z), (fun _ => 3%Z)) (2%Z, (fun _ => 3%Z)) *)
    econstructor 1. (* Use LStep_Int *)
    constructor.
    trivial.
Qed.

(* lstep (mod (fact 3) 2, s) (0, s) *) 
Example trivial4: forall s, lstep ((EMod (EFact (EInt 3%Z)) (EInt 2%Z)), s) (0%Z, s).
Proof.
Admitted.

(* (mod (fact 3) 2, s) --> (mod 3! 2, s) *) 
Example sstrivial1: forall s,  ((EMod (EFact (EInt 3%Z)) (EInt 2%Z)), s) --> ((EMod (EInt (Int_fact 3%Z)) (EInt 2%Z)), s).
Proof.
Admitted.


Example two_gt_zero: (2>0)%Z.
Proof.
eapply Z.lt_gt.
apply Z.lt_0_2.
Qed.

Eval compute in  (Int_mod 3%Z 2%Z).

Lemma three_mod_two:  (Int_mod 3%Z 2%Z = 1%Z).
Proof.
apply Zmod_odd.    
Qed.

Example six_mod_three : (Int_mod 6%Z 3%Z = 0%Z).
Proof.
  unfold Int_mod.
  apply (Z_mod_mult 2 3).
Qed.

Example trivial: forall s, lstep ((EMod (EInt 3%Z) (EInt 2%Z)), s) (1%Z, s).
Proof.
  intros.
  assert (H:=three_mod_two).
  rewrite <- H.
  econstructor.
  apply two_gt_zero.
  constructor.
  constructor.
Qed.
