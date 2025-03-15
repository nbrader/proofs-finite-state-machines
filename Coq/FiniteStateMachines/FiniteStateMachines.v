Require Import Coq.Program.Basics.
Require Import Coq.Program.Combinators.

Definition c1 : Type -> Type := fun x => x.

Definition c2 : Type := c1 bool.
