%% We have to parse COM)E into a orbit(COM, E)

use_module(library(pio)).
set_prolog_flag(double_quotes, chars).
%% use_module(library(dcg)).
use_module(library(dcg/basics)).

read_orbits(File, Pattern) :-
    phrase_from_file(match(Pattern), File).

%% read_orbits('6.example', X).

match(Pattern) -->
    string(_),
    string(41),
    remainder(_).

eol --> [C], { char_type(C, end_of_line) }, eol.
eol --> [].

planet_name(Planet) -->
    string_without(")", Planet).

orbit_description(Planet, Moon) -->
    planet_name(Planet),
    %% string(41),
    ")"
    planet_name(Moon),
    eol.

%% Example call
%% pharse(orbit_descripton(X, Y), "COM)B")


%% This is the extended orbit
orbit(X, Y) :-
    orbit(X, Z),
    orbit(Z, Y).
