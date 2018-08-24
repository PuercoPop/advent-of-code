possible_triangle(X,Y,Z) :-
    (X + Y) > Z.

possible_triangle(X,Y,Z) :-
    (X + Z) > Y.

possible_triangle(X,Y,Z) :-
    (Z+ Y) > X.
