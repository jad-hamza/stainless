Require Import Coq.Program.Tactics.
Require Import Coq.Program.Program.
Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import Omega.
Require Import ZArith.
Require Import stdpp.set.

Require Equations.Equations.

Open Scope bool_scope.
Open Scope Z_scope.

Axiom unsupported: False. 
Axiom map_type: Type -> Type -> Type.
Axiom ignore_termination: nat.

Definition magic (T: Type): T := match unsupported with end.
Set Default Timeout 60.

  
Ltac fast :=
  cbn -[Z.add] in * ||
  intros ||
  subst ||
  intuition ||
  autorewrite with libR in * ||
  congruence ||
  discriminate ||
  done ||
  autounfold in *
.

Ltac slow :=
  omega || ring || eauto.

Ltac libStep := match goal with
  | _ => progress fast
  | |- (S ?T <= ?T)%nat =>
    unify T ignore_termination; apply False_ind; exact unsupported
  | [ H: ex _ _ |- _ ] => destruct H
  |   H: exists _, _ |- _ => destruct H
  | [ |- context[match ?t with _ => _ end]] =>
      let matched := fresh "matched" in
      destruct t eqn:matched
  | [ H: context[match ?t with _ => _ end] |- _ ] =>
      let matched := fresh "matched" in
      destruct t eqn:matched
  end.
  


Inductive Marked {T}: T -> string -> Type :=
  Mark: forall t s, Marked t s
.

Ltac clearMarked :=
  repeat match goal with
         | H: Marked _ _ |- _ => clear H
         end.

Ltac clearUseless :=
  repeat match goal with
         | H: ?t = ?t |- _ => clear H
         | _ => clearMarked
         end.

Ltac isThere P :=
  match goal with
  | H: ?Q |- _ => unify P Q
  end.

Ltac termNotThere p :=
  let P := type of p in
  tryif (isThere P) then fail else idtac.


Ltac poseNew E := termNotThere E; pose proof E.


Ltac program_simplify :=
  cbn -[Z.add]; intros ; destruct_all_rec_calls ; repeat (destruct_conjs; simpl proj1_sig in * );
  subst*; autoinjections ; try discriminates ;
  try (solve [ red ; intros ; destruct_conjs ; autoinjections ; discriminates ]).

Ltac program_simpl := program_simplify ; try typeclasses eauto with program ; try program_solve_wf.

Ltac destruct_refinement :=
  match goal with
  | |- context[proj1_sig ?T] =>
    let res := fresh "RR" in
    destruct T eqn:res
  | H: context[proj1_sig ?T] |- _ =>
    let res := fresh "RR" in
    destruct T eqn:res
  end.

(*
Theorem proj1: forall P Q: Prop, P /\ Q -> P.
  intros P Q H.
  inversion H.
  apply H0. Qed.

Theorem and_left : forall (P Q : Prop),
  (P /\ Q) -> P.
Proof.
  intros P Q P_and_Q.
  destruct P_and_Q.
  exact H.
Qed. 
*)