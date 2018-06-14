%change_directory('Z:/ia02').
%consult('projet').
%------------------------------------Grille de Test
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
  4,5,6,7,_,9,1,_,3,
  7,8,9,1,2,3,4,5,_,
  2,_,4,5,6,7,8,9,1,
  5,6,7,_,9,1,2,3,4,
  8,9,1,2,3,4,5,6,7,
  3,4,5,6,7,8,9,1,2,
  _,7,_,9,1,2,_,4,5,
  9,1,2,3,4,5,6,7,8
  ].

  liste4(A):- A=[
  1,2,_,4,5,6,7,_,9,
  4,_,_,_,8,_,1,2,3,
  7,_,9,1,2,3,_,5,_,
  2,3,_,5,_,7,8,_,1,
  5,_,7,_,9,1,2,_,4,
  8,_,1,_,_,_,5,6,7,
  3,_,_,6,7,_,9,_,2,
  6,_,8,9,1,_,3,_,5,
  9,1,2,3,_,5,6,7,8
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

element(X,L) :- element(X,L,_).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

generer(A, [A|B], B).
generer(A, [B|C], [B|D]) :- generer(A,C,D).

valeur(X):- generer(X,[1,2,3,4,5,6,7,8,9],_). %capable de générer un X qui prend des valeurs de 1 à 9

permute([], []).
permute([X|Rest], L) :-
    permute(Rest, L1),
    select(X, L, L1).


pleine([]).
pleine([T|Q]) :- \+ var(T), pleine(Q).

dif(X,Y) :- X \== Y.

unique([]).
unique([T|Q]) :- maplist(dif(T), Q), unique(Q).

domaine([]).
domaine([T|Q]) :- var(T), domaine(Q), !.
domaine([T|Q]) :- T<10, T>0, domaine(Q).

etatFinal(L) :- valide(L), pleine(L).


%--------------------------Tentative pour générer une grille résolvable--------------------------

%générer une liste de nombre allant de 1 à 9
genere_liste(0,[]):- !.
genere_liste(UB, [UB|D]):- N is UB - 1, genere_liste(N,D).

liste_1_9(L):- genere_liste(9,L).

%retirer un élément d'une liste
retire_el([],_,[]).
retire_el([X|Q],X,Q):-!.
retire_el([T|Q],X,[T|D]):- retire_el(Q,X,D).

%générer une Ligne de longueur N (Nous mettrons 9 en longueur pour chacune de nos lignes)
genere_ligne(0,L,[],[]):-!.
genere_ligne(N,ListeNbs,[X|L],NewListeNbs2):- element(X,ListeNbs), retire_el(ListeNbs,X,NewListeNbs),
                                              M is N - 1 ,genere_ligne(M, NewListeNbs, L, NewListeNbs2).


%Générer une grille de sudoku pleine
/*grille(N,C):- UB is N*N, liste_1_9(Nbs), grille(N,C,Nbs).

grille(_,[],[]).
grille(N,[L|C],Nbs):- genere_ligne(N,Nbs,L,NewNbs), grille(N,C,NewNbs).*/

add(X, L, [X|L]).
add_list([], L, L).
add_list([T|Q], L, L1) :- add(T, L2, L1), add_list(Q, L, L2).

/*grille(0,[]):-!.
grille(N,Res):- liste_1_9(Nbs), genere_ligne(N,Nbs,L,NewNbs), add_list(L,Res,R),
            M is N - 1, solve(R,R2), grille(M,R2).*/
%grille(S):- S = [],!.

grille(0,[]):-!.

%grille(S):- length(S,10).

grille(N,[T|Q]) :- random(1,9,Elt), T is Elt, M is N -1, grille(M,Q).
grille(S):- grille(81,S).

/*Le début fonctionne mais impossible de concaténer 9 listes pour l'instant mais avec les prédicats
en console je peux en concaténer deux */


%----------------------------------------sudoku player
sudoku :- nl,
	write('------------------------------------------------------------------'),nl,
	write('======= Programme Sudoku - Projet IA02 - Touzeau / Rolando ======='),nl,
	write('-------------------------------------------------------------------'),nl,nl,
	repeat, menu, !.
menu :- write('\t\t=====  MENU  ====='),nl,nl,
	write(' Que voulez vous faire ?'),nl,nl,
	write('1. Resoudre un sudoku'),nl,
	write('2. Proposer un sudoku'),nl,
	write('3. Quitter'),nl,nl,
	write('Que voulez-vous faire ? : '),
	read(Choice), nl,
	gestion_choix(Choice),
  Choice=3.

gestion_choix(1) :- write("------d'un sudoku --------"), nl.





%------------------------------------- Résolution d'un sudoku
solve(Puzzle,Solution) :-
Solution = Puzzle,
Puzzle =[
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
  /* ligne */
  Ligne1 = [S11,S12,S13,S14,S15,S16,S17,S18,S19],
  Ligne2 = [S21,S22,S23,S24,S25,S26,S27,S28,S29],
  Ligne3 = [S31,S32,S33,S34,S35,S36,S37,S38,S39],
  Ligne4 = [S41,S42,S43,S44,S45,S46,S47,S48,S49],
  Ligne5 = [S51,S52,S53,S54,S55,S56,S57,S58,S59],
  Ligne6 = [S61,S62,S63,S64,S65,S66,S67,S68,S69],
  Ligne7 = [S71,S72,S73,S74,S75,S76,S77,S78,S79],
  Ligne8 = [S81,S82,S83,S84,S85,S86,S87,S88,S89],
  Ligne9 = [S91,S92,S93,S94,S95,S96,S97,S98,S99],
  /* colonne */
  Colonne1 = [S11,S21,S31,S41,S51,S61,S71,S81,S91],
  Colonne2 = [S12,S22,S32,S42,S52,S62,S72,S82,S92],
  Colonne3 = [S13,S23,S33,S43,S53,S63,S73,S83,S93],
  Colonne4 = [S14,S24,S34,S44,S54,S64,S74,S84,S94],
  Colonne5 = [S15,S25,S35,S45,S55,S65,S75,S85,S95],
  Colonne6 = [S16,S26,S36,S46,S56,S66,S76,S86,S96],
  Colonne7 = [S17,S27,S37,S47,S57,S67,S77,S87,S97],
  Colonne8 = [S18,S28,S38,S48,S58,S68,S78,S88,S98],
  Colonne9 = [S19,S29,S39,S49,S59,S69,S79,S89,S99],
  /* carré */
  Carre1 = [S11,S12,S13,S21,S22,S23,S31,S32,S33],
  Carre2 = [S14,S15,S16,S24,S25,S26,S34,S35,S36],
  Carre3 = [S17,S18,S19,S27,S28,S29,S37,S38,S39],
  Carre4 = [S41,S42,S43,S51,S52,S53,S61,S62,S63],
  Carre5 = [S44,S45,S46,S54,S55,S56,S64,S65,S66],
  Carre6 = [S47,S48,S49,S57,S58,S59,S67,S68,S69],
  Carre7 = [S71,S72,S73,S81,S82,S83,S91,S92,S93],
  Carre8 = [S74,S75,S76,S84,S85,S86,S94,S95,S96],
  Carre9 = [S77,S78,S79,S87,S88,S89,S97,S98,S99],

  Sets = [Ligne1,Ligne2,Ligne3,Ligne4,Ligne5,Ligne6,Ligne7,Ligne8,Ligne9,
          Colonne1,Colonne2,Colonne3,Colonne4,Colonne5,Colonne6,Colonne7,Colonne8,Colonne9,
          Carre1,Carre2,Carre3,Carre4,Carre5,Carre6,Carre7,Carre8,Carre9],
  maplist(permute([1,2,3,4,5,6,7,8,9]), Sets).

disp(T) :- imprime(T,1).

imprime([],_).
imprime([T|Q],I) :- 0 is mod(I,27), I \= 81, writeCase(T),nl, write('= = = = = = = = = = = = = = ='),nl, N is I+1, imprime(Q,N).
imprime([T|Q],I) :- 0 is mod(I,9), writeCase(T), nl, N is I+1, imprime(Q,N).
imprime([T|Q],I) :- 0 is mod(I,3), writeCase(T),write('|'), N is I+1, imprime(Q,N).
imprime([T|Q],I) :- writeCase(T), N is I+1, imprime(Q,N).

writeCase(X) :- var(X), write(' - '),!.
writeCase(X) :- write(' '),write(X),write(' ').
