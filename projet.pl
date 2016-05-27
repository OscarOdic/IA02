boardKahn([[(2,n),(2,n),(3,n),(1,n),(2,n),(2,n)],[(1,n),(3,n),(1,n),(3,n),(1,n),(3,n)],[(3,n),(1,n),(2,n),(2,n),(3,n),(1,n)],[(2,n),(3,n),(1,n),(3,n),(1,n),(2,n)],[(2,n),(1,n),(3,n),(1,n),(3,n),(2,n)],[(1,n),(3,n),(2,n),(2,n),(1,n),(3,n)]]).

initBoard(Board):- boardKahn(TmpBoard),
		write(`Entrez le nombre de joueurs : `),
		read(X),
		joue(X),
		affiche(TmpBoard),
		write(`Joueur 1, entrez une orientation[h/b/g/d] : `),
		read(Y),
		oriente(TmpBoard, B, Y),
		affiche(B),
		saisie_pion(B, B2, r, 6),
		saisie_pion(B2, Board, o, 6),
		!.

jeu:-initBoard(Board), affiche(Board).

:-dynamic(joueur/1).
joue(0).
joue(1):-asserta(joueur(A)).
joue(2):-asserta(joueur(A)),asserta(joueur(B)).

concat([], L, L):-!.
concat([T|Q],L,[T|R]):-concat(Q,L,R).

inverse([], []):-!.
inverse([T|Q], B):- inverse(Q,B1), concat(B1, [T], B).

oriente([], [], _):-!.
oriente([[]|_],[],_):-!.
oriente(T, T, b).
oriente([T|Q], Board, h):-inverse(T,T2), oriente(Q,Q2,h), concat(Q2,[T2], Board).
oriente(T, Board, g):-retireColonne(T,C,Tmp), oriente(Tmp, Board2, g), concat(Board2,[C],Board).
oriente(T, Board, d):-oriente(T,Board2,g), oriente(Board2, Board, h).

retireColonne([],[],[]):-!.
retireColonne([T|Q],C,NewTable):-retireColonne(Q,C2,NewTable2), premierElement(T,P), concat(P,C2,C), finList(T,F), concat([F],NewTable2,NewTable).

premierElement([],[]):-!.
premierElement([T|Q],[T]).
finList([],[]):-!.
finList([T|Q],Q).

afficheLine(0):-nl, !.
afficheLine(N):-write(' ---'), N2 is N-1, afficheLine(N2).

affiche([]):-write(' '), afficheLine(6), !.
affiche([T|Q]):-write(' '), afficheLine(6), write(' | '), imprime(T), nl, affiche(Q).

imprime([]).
imprime([T|Q]):-imprime_case(T), write(' | '), imprime(Q).

imprime_case((N,n)):-write(N), write('  '),!.
imprime_case((N,T)):-write(N), write(T).


/*=============================================================================
Placement_pièces
*/
saisie_pion(Board, Board, _, 0):-!.
saisie_pion(Board, Res, r, 6):-
	repeat,
	write('Entrez la ligne de votre Kalista : '),
	read(L),
	L=<6, L>=5,
	write('Entrez la colonne de votre Kalista : '),
	read(C),
	C=<6, C>=1,
	ajouter-pion(Board, R, rk, (L,C)),
	affiche(R),
	saisie_pion(R, Res, r, 5),
	!.
	
saisie_pion(Board, Res, o, 6):-
	repeat,
	write('Entrez la ligne de votre Kalista : '),
	read(L),
	L=<2, L>=1,
	write('Entrez la colonne de votre Kalista : '),
	read(C),
	C=<6, C>=1,
	ajouter-pion(Board, R, ok, (L,C)),
	affiche(R),
	saisie_pion(R, Res, o, 5),
	!.
	
saisie_pion(Board, Res, r, X):-
	repeat,
	write('Entrez la ligne de votre Sbire : '),
	read(L),
	L=<6, L>=5,
	write('Entrez la colonne de votre Sbire : '),
	read(C),
	C=<6, C>=1,
	ajouter-pion(Board, R, rs, (L,C)),
	X1 is X-1,
	affiche(R),
	saisie_pion(R, Res, r, X1),
	!.
	
saisie_pion(Board, Res, o, X):-
	repeat,
	write('Entrez la ligne de votre Sbire : '),
	read(L),
	L=<2, L>=1,
	write('Entrez la colonne de votre Sbire : '),
	read(C),
	C=<6, C>=1,
	ajouter-pion(Board, R, os, (L,C)),
	X1 is X-1,
	affiche(R),
	saisie_pion(R, Res, o, X1),
	!.

ajouter-pion([[(N,n)|Ql]|Qc], [[(N,P)|Ql]|Qc], P, (1,1)):-!.
ajouter-pion(_, _, _, (1,1)):-!, fail.
ajouter-pion([[T|Q1]|Q2], [[T|R1]|R2], P, (1,Y)):-Y1 is Y-1, ajouter-pion([Q1|Q2], [R1|R2], P, (X,Y1)).
ajouter-pion([T|Q], [T|R], P, (X,Y)):-X1 is X-1, ajouter-pion(Q,R,P,(X1,Y)),!.



