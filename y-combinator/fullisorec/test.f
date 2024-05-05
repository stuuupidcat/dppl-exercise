T = Rec X. X -> (Nat -> Nat);
y =
    lambda f: (Nat -> Nat) -> Nat -> Nat.
        let g =
            lambda x: T.
            lambda a: Nat.
                f (unfold [T] x x) a
        in
            g (fold [T] g)
        ;