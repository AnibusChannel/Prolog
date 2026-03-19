% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

prefix([], _).
prefix([_ | T1], [_ | T2]) :- prefix(T1, T2).

sublist(Sub, List) :- prefix(Sub, List), !.
sublist(Sub, [_ | T]) :- sublist(Sub, T).