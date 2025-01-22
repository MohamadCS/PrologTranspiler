

MaxItem(List) :: (

    Max <- (X,Y) => ( if #1 >= #2 then #1 else #2) ;
    List <- [L | Ls];

    if L <- Length(Ls) | L = 0 then ( 
        L   
    ) else (
        T <- MaxItem(Ls);
        Max(T,L)
    )
)
.











