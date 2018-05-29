%change_directory('Z:/ia02').
%consult('projet').

liste(L):-L=[[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o]].

imprime(T) :- imprimeGrille(T,1).


imprimeGrille([],I).
imprimeGrille([T|Q],I) :- 0 is mod(I,3),imprimeListe(T,1),nl, write('- - - - - -'),nl, N is I+1, imprimeGrille(Q,N).
imprimeGrille([T|Q],I) :- imprimeListe(T,1),nl, N is I+1, imprimeGrille(Q,N).

imprimeListe([],I).
imprimeListe([T|Q],I) :- 0 is mod(I,3), write(T),write('|'), N is I+1, imprimeListe(Q,N).
imprimeListe([T|Q],I) :- write(T), N is I+1, imprimeListe(Q,N).

concat([],L,L).
concat([T|Q],L,[T|R]) :- concat(Q,L,R).

ligne(L,N,R) :- recupLigne(L,N,R,1).

recupLigne([T|Q],N,L,I) :- write(N), write(I), nl, N=I, recupLigne(Q,N,T,A), !.
recupLigne([T|Q],N,L,I) :- write(N), write(I), nl, A is I+1, recupLigne(Q,N,L,A).


