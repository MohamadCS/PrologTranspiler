



First(List,X) :: (
    if length(List ,0) then (
        R <- false;
    );

    if List = [#2 | _] then (
        R <- true;
    ) else (
        R <- false;
    );

    R
) 
.
