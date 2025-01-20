
MaxItem(List) :: (

    Max <- (X,Y) => (if X >= Y then X else Y );

    List <- [L | Ls];
    if length(Ls,0) then L else Max(MaxItem(Ls),L)
)
.











