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

  listeSols(L):- L=[
    4,2,6,1,8,9,5,3,7,
    9,8,5,3,6,7,2,4,1,
    7,3,1,_,_,2,8,6,9,
    1,5,7,2,3,8,4,9,6,
    8,_,3,_,_,6,7,1,2,
    6,_,2,_,7,1,3,5,8,
    5,6,9,7,2,4,1,8,3,
    3,7,8,6,1,5,9,2,4,
    2,1,4,8,9,3,6,7,5
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

  liste_vide(L):- L=[
      _,_,_,_,_,_,_,_,_,
      _,_,_,_,_,_,_,_,_,
      _,_,_,_,_,_,_,_,_,
      _,_,_,_,_,_,_,_,_,
      _,_,_,_,_,_,_,_,_,
      _,_,_,_,_,_,_,_,_,
      _,_,_,_,_,_,_,_,_,
      _,_,_,_,_,_,_,_,_,
      _,_,_,_,_,_,_,_,_
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
%-----------------------------Créer une fonction random qui permet le backtrack

% Generate random value from Min to Max(with backtrack)
    rand_backtrack(Min,Max,RandVal):-
        create_list(Min,Max,List),
        randomize_list(List,Randomized),
        length(Randomized,Len),

    % Choose one Variable from Randomized (From first element to last).
    % When backtrack occured, next element is chosen.
        between(1,Len,Idx),
        nth1(Idx,Randomized,RandVal).

    % create integer order list
    % [Min,Min+1,Min+2,....,Max]
    create_list(Max,Max,[Max]):-!.
    create_list(Min,Max,[Min|Rest]):-
        Min1 is Min+1,
        create_list(Min1,Max,Rest).

    % shuffle List.
    % result always changes.
    % ex.randomize_list([1,2,3,4,5,6],R)     R=[4,2,6,1,3,5]
    %
    randomize_list([Val],[Val]):-!.
    randomize_list(List,[RandVal|RestRandomized]):-
        length(List,Len),
        random(1,Len,RandIdx),
        nth1(RandIdx,List,RandVal),
        select(RandVal, List, Rest),
        !,
        randomize_list(Rest,RestRandomized).

%--------------------------------Générer une grille de sudoku pleine

add(X, L, [X|L]).
add_list([], L, L).
add_list([T|Q], L, L1) :- add(T, L2, L1), add_list(Q, L, L2).

grille(0,_).
%grille(_,[T|Q]):- valide(S).
grille(N,[T|Q]) :- rand_backtrack(1,9,Elt), T is Elt, M is N - 1, grille(M,Q).
grille(S):- grille(81,S).

genere(_,0,_):- !.
genere(L,N,S):-random(1,81,X),random(1,10,Elt), replace(L,X,Elt,S), valide(S), M is N - 1, genere(S,M,S),!.
genere(L,N,S):-genere(L,N,S).

grille_valide(S,Diff):- liste_vide(L), genere(L,Diff,S).


isPlayableCell(N,Sudoku):- element(X,Sudoku,N), var(X),!.
isPlayableCell(_,_):- write('Cette case n\'est pas vide!'),nl,fail.

validUserInputNumber(X):- X>0, X=<9, integer(X), !.
validUserInputNumber(_):- nl, write('Mauvaise saisie !'),nl,nl, fail.


exit(_,Choice) :- Choice = 4.

resetSave(_) :- retract(sudokuSave(_)),!.
resetSave(_).


grille_resolvable(N,S):- liste(L), grille_resolvable2(N,L,S).


grille_resolvable2(0,L,L):- !.
grille_resolvable2(N,L,S):- random(1,81,I), P is I-1,replace(L,P,_,R), M is N - 1, grille_resolvable2(M,R,S).


%----------------------------------------sudoku player
sudoku :- nl,
  write('####################################################################'),nl,
  write('======= Programme Sudoku - Projet IA02 - Touzeau / Rolando ======='),nl,
  write('####################################################################'),nl,nl,
  resetSave(_),
  repeat, menu, !.
menu :- write('\t\t#####  MENU  ######'),nl,nl,
  write(' Résolveur de Sudoku'),nl,nl,
  write('1. Entrer un sudoku'),nl,
  write('2. Résoudre un sudoku aléatoire'),nl,
  write('3. Quitter'),nl,nl,
  write('Entrez un choix svp : '),
  read(Choice), nl,
  gestion_choix(Choice),
  Choice=3.

gestion_choix(1):- write('#### RESOLUTION DE SUDOKU ####'),nl,
            listeSols(S),
            resetSave(_),
            asserta(sudokuSave(S)),
            repeat,
            userSolvingSudoku, !.

gestion_choix(2):- write('#### SUDOKU ALEATOIRE ####'),nl,nl,
            write('Nombre de cases manquantes :'),
            read(Choice), nl,
            grille_resolvable(Choice,S),
            resetSave(_),
            asserta(sudokuSave(S)),
            repeat,
            userSolvingSudoku, !.

gestion_choix(3):- write('---- Hasta la vista, baby ! ----'),!.
gestion_choix(_):- write('---- Aucune option ne correspond à la commande entrée ----'),!.

userSolvingSudoku :- nl,write('#### Remplissez le sudoku ####'),nl,nl,
   sudokuSave(S), disp(S), nl,
  write('1. Entrer nombre'), nl,
  write('2. Supprimer nombre'), nl,
  write('3. Résoudre'), nl,
  write('4. Quitter'), nl, nl,
  write('Entrez un choix svp : '),
  read(Choice), nl,
  fillSudoku(Choice,S),
  sudokuSave(NewS),
  exit(NewS,Choice).
% ------------------------------------ Gestion user


fillSudoku(1,S):- write('Ligne de la case à remplir : '), read(X), validUserInputNumber(X),nl,
  write('Colonne de la case à remplir :'), read(Y), validUserInputNumber(Y),nl,
  write('Nombre de la case : '), read(Z), validUserInputNumber(Z),nl,
  I is ((X-1)*9)+(Y-1),
  replace(S,I,Z,S1),
  resetSave(_),
  asserta(sudokuSave(S1)),
  write('Succès de l\'ajout.'),
  nl,!.

fillSudoku(1,_):- !.

fillSudoku(2,S):- write('Ligne de la case à supprimer :'), read(X), validUserInputNumber(X),nl,
  write('Colonne de la case à supprmier:'), read(Y), validUserInputNumber(Y),nl,
  I is ((X-1)*9)+(Y-1),
  replace(S,I,_,S1),
  resetSave(_),
  asserta(sudokuSave(S1)),!.

fillSudoku(2,_):- !.

fillSudoku(3,S):-
  solve(S,S1),
  resetSave(_),
  asserta(sudokuSave(S1)),!.

fillSudoku(3,_):- !.

fillSudoku(4,_):- write('Retour au menu'), nl,!.

fillSudoku(_,_):- nl, write('Option invalide'), nl.


%-----------------------------------------Validité d'une Grille
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

imprime([],_):- !.
imprime([T|Q],I) :- 0 is mod(I,27), I \= 81, writeCase(T),nl, write('= = = = = = = = = = = = = = ='),nl, N is I+1, imprime(Q,N),!.
imprime([T|Q],I) :- 0 is mod(I,9), writeCase(T), nl, N is I+1, imprime(Q,N),!.
imprime([T|Q],I) :- 0 is mod(I,3), writeCase(T),write('|'), N is I+1, imprime(Q,N),!.
imprime([T|Q],I) :- writeCase(T), N is I+1, imprime(Q,N),!.

writeCase(X) :- var(X), write(' - '),!.
writeCase(X) :- write(' '),write(X),write(' ').
