c
instructions([], Instructions) :- Instructions.
instructions([C|Odes], Instructions) :-
    char_code(I, C),
    instructions(Odes, [I|Instructions]).

solve :- open('./3.input', read, Stream),
         read_stream_to_codes(Stream, Codes),
         close(Stream),
         instructions(Codes, Foo),
         write(Foo).
