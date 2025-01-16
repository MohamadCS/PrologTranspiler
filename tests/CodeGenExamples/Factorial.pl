

Factorial(N) ::(
    if N =:= 1; N =:= 0 then (
        R is 1;
    ) else (
        N1 is N-1;
        X <- Factorial(N1);
        R is (X * N);
    );
    R
).

