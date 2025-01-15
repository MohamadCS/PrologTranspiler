



max(X,Y,X) :- X >= Y.
max(X,Y,Y) :- Y > X. 


MaxList(List) :: (
    List = [L | Ls];
    Current <- MaxList(Ls);
    max(Current,L, R);
    R
).


