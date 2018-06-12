%change_directory('Z:/ia02').
%consult('projet').

liste2(L):- L=[
    _,_,5,3,_,_,_,_,_,
    8,_,_,_,_,_,_,2,_,
    _,7,_,_,1,_,5,_,_,
    4,_,_,_,_,5,3,_,_,
    _,1,_,_,7,_,_,_,6,
    _,_,3,2,_,_,_,8,_,
    _,6,_,5,_,_,_,_,9,
    _,_,4,_,_,_,_,3,_,
    _,_,_,_,_,9,7,_,_
  ].

    liste3(A):- A=[
  1,2,_,4,5,6,7,8,9,
  4,5,6,7,_,9,1,2,3,
  7,8,9,1,2,3,4,5,6,
  2,3,4,_,6,7,8,9,1,
  5,6,7,8,9,1,2,3,4,
  8,9,1,2,_,4,5,6,7,
  3,4,5,6,7,8,9,1,2,
  6,7,8,9,1,2,3,4,5,
  9,1,2,3,4,5,6,7,8
  ].


  liste(A):- A=[
  1,2,3,4,5,6,7,8,9,
  4,5,6,7,8,9,1,2,3,
  7,8,9,1,2,3,4,5,6,
  2,3,4,5,6,7,8,9,1,
  5,6,7,8,9,1,2,3,4,
  8,9,1,2,3,4,5,6,7,
  3,4,5,6,7,8,9,1,2,
  6,7,8,9,1,2,3,4,5,
  9,1,2,3,4,5,6,7,8
  ].

concat([],L,L).
concat([T|Q],L,[T|R]) :- concat(Q,L,R).

element(X,[X|_],0).
element(X,[_|Q],R) :- element(X,Q,N), R is N+1.

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

generer(A, [A|B], B).
generer(A, [B|C], [B|D]) :- generer(A,C,D).

valeur(X):- generer(X,[1,2,3,4,5,6,7,8,9],_). %capable de générer un X qui prend des valeurs de 1 à 9

sudoku(S,Res) :- sudoku(S,Res,0).

sudoku([],Res,I) :- etatFinal(Res).
sudoku([T|Q],Res,I) :- valeur(X), T is X, replace(Res,I,X,Resu), valide(Resu) ,N is I+1 ,sudoku(Q,Resu,N).
sudoku([T|Q],R,I) :- N is I+1,  sudoku(Q,R,I).



pleine([]).
pleine([T|Q]) :- \+ var(T), pleine(Q).

dif(X,Y) :- X \== Y.

unique([]).
unique([T|Q]) :- maplist(dif(T), Q), unique(Q).

domaine([]).
domaine([T|Q]) :- var(T), domaine(Q), !.
domaine([T|Q]) :- T<10, T>0, domaine(Q).

etatFinal(L) :- valide(L), pleine(L).

valide(L) :- L =[
    S11,S12,S13,S14,S15,S16,S17,S18,S19,
    S21,S22,S23,S24,S25,S26,S27,S28,S29,
    S31,S32,S33,S34,S35,S36,S37,S38,S39,
    S41,S42,S43,S44,S45,S46,S47,S48,S49,
    S51,S52,S53,S54,S55,S56,S57,S58,S59,
    S61,S62,S63,S64,S65,S66,S67,S68,S69,
    S71,S72,S73,S74,S75,S76,S77,S78,S79,
    S81,S82,S83,S84,S85,S86,S87,S88,S89,
    S91,S92,S93,S94,S95,S96,S97,S98,S99
],
/* Test du domaine */
domaine(L),
/* ligne */
unique([S11,S12,S13,S14,S15,S16,S17,S18,S19]),
unique([S21,S22,S23,S24,S25,S26,S27,S28,S29]),
unique([S31,S32,S33,S34,S35,S36,S37,S38,S39]),
unique([S41,S42,S43,S44,S45,S46,S47,S48,S49]),
unique([S51,S52,S53,S54,S55,S56,S57,S58,S59]),
unique([S61,S62,S63,S64,S65,S66,S67,S68,S69]),
unique([S71,S72,S73,S74,S75,S76,S77,S78,S79]),
unique([S81,S82,S83,S84,S85,S86,S87,S88,S89]),
unique([S91,S92,S93,S94,S95,S96,S97,S98,S99]),
/* colonne */
unique([S11,S21,S31,S41,S51,S61,S71,S81,S91]),
unique([S12,S22,S32,S42,S52,S62,S72,S82,S92]),
unique([S13,S23,S33,S43,S53,S63,S73,S83,S93]),
unique([S14,S24,S34,S44,S54,S64,S74,S84,S94]),
unique([S15,S25,S35,S45,S55,S65,S75,S85,S95]),
unique([S16,S26,S36,S46,S56,S66,S76,S86,S96]),
unique([S17,S27,S37,S47,S57,S67,S77,S87,S97]),
unique([S18,S28,S38,S48,S58,S68,S78,S88,S98]),
unique([S19,S29,S39,S49,S59,S69,S79,S89,S99]),
/* carré */
unique([S11,S12,S13,S21,S22,S23,S31,S32,S33]),
unique([S14,S15,S16,S24,S25,S26,S34,S35,S36]),
unique([S17,S18,S19,S27,S28,S29,S37,S38,S39]),
unique([S41,S42,S43,S51,S52,S53,S61,S62,S63]),
unique([S44,S45,S46,S54,S55,S56,S64,S65,S66]),
unique([S47,S48,S49,S57,S58,S59,S67,S68,S69]),
unique([S71,S72,S73,S81,S82,S83,S91,S92,S93]),
unique([S74,S75,S76,S84,S85,S86,S94,S95,S96]),
unique([S77,S78,S79,S87,S88,S89,S97,S98,S99]).

disp(T) :- imprime(T,1).

imprime([],_).
imprime([T|Q],I) :- 0 is mod(I,27), I \= 81, writeCase(T),nl, write('= = = = = = = = = = = = = = ='),nl, N is I+1, imprime(Q,N).
imprime([T|Q],I) :- 0 is mod(I,9), writeCase(T), nl, N is I+1, imprime(Q,N).
imprime([T|Q],I) :- 0 is mod(I,3), writeCase(T),write('|'), N is I+1, imprime(Q,N).
imprime([T|Q],I) :- writeCase(T), N is I+1, imprime(Q,N).

writeCase(X) :- var(X), write(' - '),!.
writeCase(X) :- write(' '),write(X),write(' ').


