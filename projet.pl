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
		ajout_pions(B, B2, r),
		ajout_pions(B2, Board, o),
		!.

jeu:-initBoard(Board), affiche(Board).

:-dynamic(joueur/1).
joue(0).
joue(1):-asserta(joueur(r)).
joue(2):-asserta(joueur(r)),asserta(joueur(o)).

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
afficheLine(N):-write(' -------'), N2 is N-1, afficheLine(N2).

affiche([]):-write(' '), afficheLine(6), !.
affiche([T|Q]):-write(' '), afficheLine(6), write(' | '), imprime(T), nl, affiche(Q).

imprime([]).
imprime([T|Q]):-imprime_case(T), write(' | '), imprime(Q).

imprime_case((N,n)):-write(N), write('    '),!.
imprime_case((N,T)):-write(N), write(' '), write(T).


/*=============================================================================
Placement_pieces
*/

ajout_pions(B, Board, r):-
	joueur(r),
	write('Joueur rouge, saisissez vos pions'), nl,
	saisie_pion(B, B1, rka, r),
	saisie_pion(B1, B2, rs1, r),
	saisie_pion(B2, B3, rs2, r),
	saisie_pion(B3, B4, rs3, r),
	saisie_pion(B4, B5, rs4, r),
	saisie_pion(B5, Board, rs5, r),
	!.

ajout_pions(B, Board, r):-
	write('Ajout des pions de l''IA rouge...'), nl,
	ajout_pion_IA(B, B1, rka, r),
	ajout_pion_IA(B1, B2, rs1, r),
	ajout_pion_IA(B2, B3, rs2, r),
	ajout_pion_IA(B3, B4, rs3, r),
	ajout_pion_IA(B4, B5, rs4, r),
	ajout_pion_IA(B5, Board, rs5, r),
	write('Terminé'), nl,
	!.

ajout_pions(B, Board, o):-
	joueur(o),
	write('Joueur ocre, saisissez vos pions'), nl,
	saisie_pion(B, B1, oka, o),
	saisie_pion(B1, B2, os1, o),
	saisie_pion(B2, B3, os2, o),
	saisie_pion(B3, B4, os3, o),
	saisie_pion(B4, B5, os4, o),
	saisie_pion(B5, Board, os5, o),
	!.

ajout_pions(B, Board, o):-
	write('Ajout des pions de l''IA ocre...'), nl,
	ajout_pion_IA(B, B1, oka, o),
	ajout_pion_IA(B1, B2, os1, o),
	ajout_pion_IA(B2, B3, os2, o),
	ajout_pion_IA(B3, B4, os3, o),
	ajout_pion_IA(B4, B5, os4, o),
	ajout_pion_IA(B5, Board, os5, o),
	write('Terminé'), nl,
	!.

saisie_pion(Board, Res, rka, r):-
	repeat,
	write('Entrez la ligne de votre Kalista : '),
	read(L),
	L=<6, L>=5,
	write('Entrez la colonne de votre Kalista : '),
	read(C),
	C=<6, C>=1,
	ajouter_pion(Board, Res, rka, (L,C)),
	affiche(Res),
	!.

saisie_pion(Board, Res, X, r):-
	repeat,
	write('Entrez la ligne de votre Sbire : '),
	read(L),
	L=<6, L>=5,
	write('Entrez la colonne de votre Sbire : '),
	read(C),
	C=<6, C>=1,
	ajouter_pion(Board, Res, X, (L,C)),
	affiche(Res),
	!.

saisie_pion(Board, Res, ok, o):-
	repeat,
	write('Entrez la ligne de votre Kalista : '),
	read(L),
	L=<2, L>=1,
	write('Entrez la colonne de votre Kalista : '),
	read(C),
	C=<6, C>=1,
	ajouter_pion(Board, Res, oka, (L,C)),
	affiche(Res).

saisie_pion(Board, Res, X, o):-
	repeat,
	write('Entrez la ligne de votre Sbire : '),
	read(L),
	L=<2, L>=1,
	write('Entrez la colonne de votre Sbire : '),
	read(C),
	C=<6, C>=1,
	ajouter_pion(Board, Res, X, (L,C)),
	affiche(Res).

ajouter_pion([T|Q], [Res|Q], X, (1,C)):-ajouter_colonne(T,Res,X,C),!.
ajouter_pion([T|Q], [T|R], X, (L,C)):-Tmp is L-1, ajouter_pion(Q, R, X, (Tmp,C)).

ajouter_colonne([(N,n)|Q], [(N,X)|Q], X, 1):-!.
ajouter_colonne([T|Q], [T|R], X, C):-Tmp is C-1, ajouter_colonne(Q, R, X, Tmp).

ajout_pion_IA(B, Board, X, r):-
	repeat,
	random(1, 7, C),
	random(5, 7, L),
	ajouter_pion(B, Board, X, (L,C)).

ajout_pion_IA(B, Board, X, o):-
	repeat,
	random(1, 7, C),
	random(1, 3, L),
	ajouter_pion(B, Board, X, (L,C)).
