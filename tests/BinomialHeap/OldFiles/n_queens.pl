replace_ith_element(List,Index,NewElement,Result) :- 
    replace_ith_element(List,Index,NewElement,Result,0).


replace_ith_element([],Index,_,[],Index). 

replace_ith_element([_ | Xs],Index,NewElement,[NewElement | Xs],Index).

replace_ith_element([X | Xs],Index,NewElement,[X | Rs],CurrentIndex) :-
    NextIndex is CurrentIndex +1,
    replace_ith_element(Xs,Index,NewElement,Rs,NextIndex)
    .


verify_column(Matrix,I,J) :-  
    I >= 0,
    J >= 0,
    nth0(I,Matrix,Row),
    nth0(J,Row,0),
    LowerI is I - 1,
    (LowerI is -1 ; verify_column(Matrix,LowerI,J))
    .

verify_lower_diagonal(Matrix,I,J) :-  
    I >= 0,
    J >= 0,
    nth0(I,Matrix,Row),
    nth0(J,Row,0),
    LowerI is I - 1,
    LowerJ is J - 1,
    (LowerJ is -1 ; LowerI is -1 ; verify_lower_diagonal(Matrix,LowerI,LowerJ))
    .
    
verify_upper_diagonal(Matrix,I,J) :-  
    I >= 0,
    J >= 0,
    nth0(I,Matrix,Row),
    nth0(J,Row,0),
    length(Row,N),
    LowerI is I - 1,
    UpperJ is J + 1,
    (LowerI is -1 ; UpperJ is N ; verify_upper_diagonal(Matrix,LowerI,UpperJ))
    .



verify(N,Matrix,I,J) :-
    create_empty_row(N,EmptyRow),
    append(Matrix,[EmptyRow],NewMatrix),
    verify_column(NewMatrix,I,J),
    verify_lower_diagonal(NewMatrix,I,J),
    verify_upper_diagonal(NewMatrix,I,J)
    .

create_empty_row(N,Result) :-
    length(Result, N),
    maplist(=(0),Result)
    .

create_row(N,Index, Result) :-
    create_empty_row(N,EmptyRow ),
    replace_ith_element(EmptyRow,Index, 1, Result)
    .

try_cases(N,CurrentBoard,ResultBoard,CurrentRowNum) :-
    try_cases(N,CurrentBoard,ResultBoard,0,CurrentRowNum)
    .

try_cases(N,CurrentBoard,ResultBoard,CurrentColNum,CurrentRowNum) :-
    (
    verify(N,CurrentBoard, CurrentRowNum, CurrentColNum),
    create_row(N,CurrentColNum,Row),
    append(CurrentBoard,[Row],ResultBoard)
    );
    (
        NextCol is CurrentColNum + 1,
        NextCol < N,
        try_cases(N,CurrentBoard,ResultBoard,NextCol,CurrentRowNum)
    )
    .

n_queens(N,R) :- N1 is N-1 ,n_queens(N,N1,R).

n_queens(_, -1 ,[]).

n_queens(N, CurrentRowNum ,ResultBoard) :-
    CurrentRowNum >=0,
    LastRow is CurrentRowNum - 1,
    n_queens(N,LastRow,CurrentBoard),
    try_cases(N,CurrentBoard,ResultBoard,CurrentRowNum)
    .


n_queens_count(N,Count) :-
    findall(R, n_queens(N, R), List),
    length(List, Count)
    .









