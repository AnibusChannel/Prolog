% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

% parent(child, parent)
parent(charlie, alice).
parent(diane, alice).
parent(charlie, bob).
parent(diane, bob).
parent(gregory, emily).
parent(harry, emily).
parent(gregory, fred).
parent(harry, fred).
parent(oscar, fred).
parent(ian, diane).
parent(jack, diane).
parent(kevin, diane).
parent(ian, gregory).
parent(jack, gregory).
parent(kevin, gregory).
parent(michael, kevin).
parent(norman, kevin).
parent(michael, linda).
parent(norman, linda).

% Нововведение для ненулевых результатов
parent(linda, charlie).

woman(alice).
woman(diane).
woman(emily).
woman(linda).
man(bob).
man(charlie).
man(fred).
man(gregory).
man(harry).
man(oscar).
man(ian).
man(jack).
man(kevin).
man(michael).
man(norman).

% aunt(niece, aunt)
aunt(X, Y) :- parent(X, P), parent(P, D), parent(Y, D), woman(Y), Y \= P.
niece(X, Y) :- aunt(Y, X), woman(Y).

% Причина длительной отладки заключается в том, что в изначальной схеме таковых пар не было
