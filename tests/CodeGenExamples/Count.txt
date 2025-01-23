



Count(List,Func) :: (
    if length(List,0) then(
        0
    ) else (
        List <- [L | Ls];
        I <- Count(Ls,Func);
        Condition <- Func(L);

        if Condition then (
            I + 1
        ) else (
            I
        )
    ) 
)
.


Main() :: (
    Condition <- (X) => ( X >= 1 );
    Result <- Count([1,3,4,3],Condition);
    write(Result)
)
.
