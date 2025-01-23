

F(Func) :: (
    Func(1,2)
)
.

G1() :: (
    F((X,Y) => (X + Y))
)
.

G2() :: (
    Add <- (X,Y) => (X + Y); 
    F(Add)
)
.


