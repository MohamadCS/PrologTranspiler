Max(X,Y) ::(
    if X >= Y then (
        R <- X;
    ) else (
        R<- Y;
    );
    R
)
.


MaxItem(List) :: (
    List = [L | Ls];
    if length(Ls,0) then (
        R <- L;
    )
    else(
        T <- MaxItem(Ls);
        if T >= L then (
            R <- T;
        ) 
        else (
            R <- L;
        )
    );
    R
).











