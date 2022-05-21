%% https://stackoverflow.com/a/23268528/357198

:- [library(dcg/basics)].
:- [library(pure_input)].

sep -->
    white,
    whites.

row(A, B, C) -->
    integer(A), sep, integer(B), sep, integer(C).


%% possible_triangle(X,Y,Z) :-
%%     (X + Y) > Z.

%% possible_triangle(X,Y,Z) :-
%%     (X + Z) > Y.

%% possible_triangle(X,Y,Z) :-
%%     (Z+ Y) > X.

%% main :-
%%     open('3.input', read, Str),
%%     read(Str, Line),
%%     close(Str),
%%     Line.


%% Smoke test for grammar
%% ?- atom_codes('  566  477  376', CS), phrase(row(R), Cs).
