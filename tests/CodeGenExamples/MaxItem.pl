

F() <-- (
    Z <- write(2);
    Y <-- write(3);
    write(1);
    Y();
)
.

MaxItem(List) <-- (
    X <- F();
    Max(X,Y) <-- (if X >= Y then X else Y );
    write(1);
    List <- [L | Ls];
    if length(Ls,0) then L else Max(MaxItem(Ls),L)
)
.











