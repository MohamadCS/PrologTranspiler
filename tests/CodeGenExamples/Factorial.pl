

Factorial(N) ::(
    if N =< 1 then (
        R is 1;
    ) else (
        X <- Factorial(N-1);
        R is (X * N);
    );
    R
).

