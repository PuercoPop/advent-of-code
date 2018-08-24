
:- module(_, [walk]).

step(1,u,1).
step(1,d,4).
step(1,l,1).
step(1,r,2).

step(2,u,2).
step(2,d,5).
step(2,l,1).
step(2,r,3).

step(3,u,3).
step(3,d,6).
step(3,l,2).
step(3,r,3).

step(4,u,1).
step(4,d,7).
step(4,l,4).
step(4,r,5).

step(5,u,2).
step(5,d,8).
step(5,l,4).
step(5,r,6).

step(6,u,3).
step(6,d,9).
step(6,l,5).
step(6,r,6).

step(7,u,4).
step(7,d,7).
step(7,l,7).
step(7,r,8).

step(8,u,5).
step(8,d,8).
step(8,l,7).
step(8,r,9).

step(9,u,6).
step(9,d,9).
step(9,l,8).
step(9,r,9).

% head of path es lo que conecta uno al otro
walk(Start, Path, X) :-
    Path = [ S | Next_Path],
    step(Start, S, Next_Position),
    walk(Next_Position, Next_Path, X).

walk(Start, [], X) :- X = Start.

?- walk(5, [u,l,l], X).
