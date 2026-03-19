% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

remove_every_third(List, Result) :- remove_every_third(List, 1, Result).
remove_every_third([], _, []).
remove_every_third([H | T], N, Result) :-
    (0 is N mod 3 -> Result = Rest; Result = [H | Rest]),
    N1 is N + 1,
    remove_every_third(T, N1, Rest).