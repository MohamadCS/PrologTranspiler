Max(X,Y) ::( if X >= Y then (X) else (Y) ) .

MaxItem(List) :: (
    List <- [L | Ls];
    if length(Ls,0) then ( 
        L   
    ) else (
        T <- MaxItem(Ls);
        Max(T,L)
    )
)
.











