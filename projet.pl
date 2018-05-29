%change_directory('Z:/ia02').
%consult('projet').

liste(L):-L=[[un,o,o,o,o,o,o,o,o],[deux,o,o,o,o,o,o,o,o],[trois,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o]].

imprime(T) :- imprimeGrille(T,1).


imprimeGrille([],I).
imprimeGrille([T|Q],I) :- 0 is mod(I,3),imprimeListe(T,1),nl, write('- - - - - -'),nl, N is I+1, imprimeGrille(Q,N).
imprimeGrille([T|Q],I) :- imprimeListe(T,1),nl, N is I+1, imprimeGrille(Q,N).

imprimeListe([],I).
imprimeListe([T|Q],I) :- 0 is mod(I,3), write(T), write('|'), N is I+1, imprimeListe(Q,N).
imprimeListe([T|Q],I) :- write(T), N is I+1, imprimeListe(Q,N).

concat([],L,L).
concat([T|Q],L,[T|R]) :- concat(Q,L,R).


inverse([], []).
inverse([F|Fs], T) :- inverse(F, [F|Fs], T).
inverse([], _, []).
inverse([_|Res], L, [T|Q]) :- restList(L, T, L1), inverse(Res, L1, Q).

restList([], [], []).
restList([[F|Os]|Rest], [F|Fs], [Os|Oss]) :- restList(Rest, Fs, Oss).


recupLigne([H|_],1,H) :- !.
recupLigne([_|T],N,H) :- N > 0, N1 is N-1, recupLigne(T,N1,H).

recupColonne(L,I,H) :- inverse(L,X), recupLigne(X,I,H).



