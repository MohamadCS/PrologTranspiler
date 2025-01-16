

Fib(N) :: (
    if N =<  1 then (
        R <- N;
    ) else (
        N1 is N-1;
        N2 is N-2;
        F1 <- Fib(N1);
        F2 <- Fib(N2);
        X is (F1+F2);
        R <- X;
    );
    R
)
.

