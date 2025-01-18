

Fib(N) :: (
    if N = 0 then (
        R <- 0;
    );

    if N = 1 then (
        R <- 1;
    );

    if N > 1 then (
        F1 <- Fib(N-1);
        F2 <- Fib(N-2);
        R <- (F1 + F2); 
    );

    R
)
.

