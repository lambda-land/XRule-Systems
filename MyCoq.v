




Theorem plus_O_n : forall n : nat, 0 + n = n.
Proof.
    intros n. simpl. reflexivity.  Qed.

Theorem plus_n_Sm : forall n m : nat, S (n + m) = n + (S m).
Proof.
    induction n.
    simpl. intros. reflexivity.
    simpl. intros. rewrite IHn. reflexivity.
Qed.



Theorem reimann_hypothesis : forall (P : nat -> Prop),
    (forall n : nat, (exists k : nat, n = 4 * k) -> P n) ->
    (forall n : nat, P (n + 3)) ->
    (forall n : nat, P (n + 5)) ->
    (forall n : nat, P n).
