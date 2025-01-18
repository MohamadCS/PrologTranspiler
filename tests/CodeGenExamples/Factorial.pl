

Factorial1(N) ::(
    if N =< 1 then (
        R <- 1;
    ) else (
        X <- Factorial1(N-1);
        R <- (X * N);
    );
    R
).

Factorial2(N) ::(
    if N =< 1 then (
        R <- 1;
        R
    ) else (
        X <- Factorial2(N-1);
        R <- (X * N);
        R
    )
).
