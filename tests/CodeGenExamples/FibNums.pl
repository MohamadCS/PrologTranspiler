
Fib1(N) :: (
    if N = 0 then (
        R <- 0;
    );

    if N = 1 then (
        R <- 1;
    );

    if N > 1 then (
        F1 <- Fib1(N-1);
        F2 <- Fib1(N-2);
        R <- (F1 + F2); 
    );

    R
)
.

Fib2(N) :: (
    if N =< 1 then (
        R <- N;
    ) else (
        F1 <- Fib2(N-1);
        F2 <- Fib2(N-2);
        R <- (F1 + F2); 
    );
    R
)
.

