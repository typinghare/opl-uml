(*
 * HW2, Question 4(a): Proof for exercise basic_induction in Software Foundations, Volume 1, Chapter: Proof by Induction.
 * Exercise link: https://softwarefoundations.cis.upenn.edu/lf-current/Induction.html
 * Author: Zhuojian Chen (James)
 * Date: 02/23/2024
 *)

(*
 * Definition of natural numbers in Coq.
 * In the Peano arithmetic system, "O" denotes zero, while "S n" denotes the successor of the natural number "n".
 * Example: "O" represents zero; "S O" represents one (the sucessor of zero); "S (S O)" represents two (the sucessor of two).
 *)
(*
Inductive nat: Type :=
  | O
  | S (n : nat).
*)

(*
 * Defines the binary operator "*" using the built-in function "mult".
 *)
Notation "x * y" := (mult x y)
  (at level 40, left associativity)
  : nat_scope.

Theorem mul_0_r : forall n: nat,
  n * 0 = 0.
Proof.
  (* Introduce variable n to the context. *)
  intros n.

  (* Perform induction on n, which is a natural number. It will generate two goals for the two constructors. *)
  (* Here [| n'] is the pattern for the induction hypothesis. "|" separates the cases for induction. *)
  (* For the base case, "n" is 0 and the case is handled directly without an induction hypothesis. *)
  (* For the inductive case, "n" follows the form "S n'", so we introduce a "n'" here. *)
  (* Note that Coq will automatically introduce for us the "IHn'", which is induction hypopthesis. *)
  (* Therefore, we can say that "n'" is equivalent to "n' IHn'". *)
  (* Based on the definition of natural number, the induction hypothesis is "n' * 0 = 0". *)
  induction n as [| n'].
  (* The base case: "n" is 0. Just simplify and two sides become equal. *)
  (* Note that the "reflexivity" tactic tries to apply "simpl" tactic first, so "simpl" before it can be left out. *)
  - simpl. reflexivity.
  (* The inductive case: simply "S n'" to "n'" *)
  - simpl. rewrite IHn'. reflexivity.
Qed.
(*
 * Summary to the above proof: For all natural number n, there are two cases: (1) n is 0, then 0 * 0 = 0. (2) n is not zero, then n = S n' (the successor of another natural number n'). We hypothesize n' * 0 = 0, and we will show that n * 0 = 0. Since (S n') * 0 = n' * 0 + 1 * 0 = n' * 0 = 0.
 *)

Theorem plus_n_Sm : forall n m: nat,
  S (n + m) = n + (S m).
Proof.
  intros n m.
  induction n as [| n'].
  - reflexivity.
  (* simplification: S a + b =  S (a + b) *)
  - simpl. rewrite -> IHn'. reflexivity.
Qed.

Theorem plus_n_0: forall n: nat,
  n + 0 = n.
Proof.
  intros n.
  induction n as [| n'].
  - reflexivity.
  - simpl. rewrite -> IHn'. reflexivity.
Qed.

Theorem add_comm : forall n m: nat,
  n + m = m + n.
Proof.
  intros n m.
  induction n as [| n'].
  - simpl. rewrite -> plus_n_0. reflexivity. 
  - simpl. rewrite <- plus_n_Sm. rewrite -> IHn'. reflexivity.
Qed.

Theorem add_assoc : forall n m p: nat,
  n + (m + p) = (n + m) + p.
Proof.
  intros n m p.
  induction n as [| n'].
  - reflexivity.
  - simpl. rewrite -> IHn'. reflexivity.
Qed.