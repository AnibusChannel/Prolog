% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

append([], X, X).
append([H|T1], X, [H|T2]) :- append(T1, X, T2).

remove_brackets([], []).
remove_brackets([[] | T], R) :- remove_brackets(T, R), !.
remove_brackets([[X | Y] | T], R) :-
    remove_brackets([X | Y], RH),
    remove_brackets(T, RT),
    append(RH, RT, R), !.
remove_brackets([H | T], [H | RT]) :- remove_brackets(T, RT).
