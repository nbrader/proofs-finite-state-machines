Require Import Coq.Program.Basics.
Require Import Coq.Program.Combinators.

Definition c1 : Type -> Type := fun x => x.

Definition c2 : Type.
Definition c2 := c1 (c1 bool).

Definition t1 : Type := c1 bool.
