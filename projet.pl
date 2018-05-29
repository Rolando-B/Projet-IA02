%change_directory('Z:/ia02').
%consult('projet').

liste(L):-L=[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o].

disp(T) :- imprime(T,1).

imprime([],I).
imprime([T|Q],I) :- 0 is mod(I,27), write(T),nl, write('- - - - - -'),nl, N is I+1, imprime(Q,N).
imprime([T|Q],I) :- 0 is mod(I,9), write(T), nl, N is I+1, imprime(Q,N).
imprime([T|Q],I) :- 0 is mod(I,3), write(T),write('|'), N is I+1, imprime(Q,N).
imprime([T|Q],I) :- write(T), N is I+1, imprime(Q,N).

concat([],L,L).
concat([T|Q],L,[T|R]) :- concat(Q,L,R).

recupLigne([],N,R).
recupLigne(L,N,R) :- ligne(L,1,1,N,R).




ligne([],I,J,K,R).
ligne([T|Q],I,J,K,R) :- 0 is mod(I,9),I2 is I+1,J2 is J+1, ligne(Q,I2,J2,K,R).
ligne([T|Q],I,J,K,R) :- J =:= K,concat(T,R,R), I2 is I+1, ligne(Q,I2,J,K,R).
ligne([T|Q],I,J,K,R) :- I2 is I+1,ligne(Q,I2,J,K,R).
ligne([T|Q],I,J,K,R) :- J > K, !, fail.
