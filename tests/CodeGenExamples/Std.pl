

module std {
pub ForEach(List,Func) :: (
    if length(List,0) then (
            []
        ) else (
            List = [L | Ls];
            Rs <- ForEach(Ls,Func);
            R  <- Func(L);
            [R | Rs]
    )
)
}



