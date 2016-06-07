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

jeu:-initBoard(Board), affiche(Board), tours(Board, r, n),!.

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
premierElement([T|_],[T]).
finList([],[]):-!.
finList([_|Q],Q).

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
	ajouter_pion_libre(Board, Res, rka, (L,C)),
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
	ajouter_pion_libre(Board, Res, X, (L,C)),
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
	ajouter_pion_libre(Board, Res, oka, (L,C)),
	affiche(Res).

saisie_pion(Board, Res, X, o):-
	repeat,
	write('Entrez la ligne de votre Sbire : '),
	read(L),
	L=<2, L>=1,
	write('Entrez la colonne de votre Sbire : '),
	read(C),
	C=<6, C>=1,
	ajouter_pion_libre(Board, Res, X, (L,C)),
	affiche(Res).

ajouter_pion_libre([T|Q], [Res|Q], X, (1,C)):-ajouter_colonne_init(T,Res,X,C),!.
ajouter_pion_libre([T|Q], [T|R], X, (L,C)):-Tmp is L-1, ajouter_pion_libre(Q, R, X, (Tmp,C)).

ajouter_colonne_init([(N,n)|Q], [(N,X)|Q], X, 1):-!.
ajouter_colonne_init([T|Q], [T|R], X, C):-Tmp is C-1, ajouter_colonne_init(Q, R, X, Tmp).

ajouter_pion([T|Q], [Res|Q], X, (1,C)):-ajouter_colonne(T,Res,X,C),!.
ajouter_pion([T|Q], [T|R], X, (L,C)):-Tmp is L-1, ajouter_pion(Q, R, X, (Tmp,C)).

ajouter_colonne([(N,_)|Q], [(N,X)|Q], X, 1):-!.
ajouter_colonne([T|Q], [T|R], X, C):-Tmp is C-1, ajouter_colonne(Q, R, X, Tmp).

ajout_pion_IA(B, Board, X, r):-
	repeat,
	random(1, 7, C),
	random(5, 7, L),
	ajouter_pion_libre(B, Board, X, (L,C)).

ajout_pion_IA(B, Board, X, o):-
	repeat,
	random(1, 7, C),
	random(1, 3, L),
	ajouter_pion_libre(B, Board, X, (L,C)).

retirer_pion([T|Q], [Res|Q], (1,C)):-retirer_colonne(T,Res,C),!.
retirer_pion([T|Q], [T|R], (L,C)):-Tmp is L-1, retirer_pion(Q, R, (Tmp,C)).

retirer_colonne([(N,_)|Q], [(N,n)|Q], 1):-!.
retirer_colonne([T|Q], [T|R], C):-Tmp is C-1, retirer_colonne(Q, R, Tmp).

/*=============================================================================
Mouvements possible
*/

taille([_], 1):-!.
taille([_|Q], N):-taille(Q, Tmp), N is Tmp + 1.

element([T|_], Res, (1,C)):-element_colonne(T, Res, C),!.
element([_|Q], Res, (L,C)):-Tmp is L-1, element(Q, Res, (Tmp, C)),!.

element_colonne([T|_], T, 1):-!.
element_colonne([_|Q], Res, C):-Tmp is C-1, element_colonne(Q, Res, Tmp).

present([T|_], T):-!.
present([_|Q], D):-present(Q, D).

libre(Board, Coord):-element(Board, (_,n), Coord).

possible(Board, Coord, r):-element(Board, (_,T), Coord), T\=rka, T\=rs1, T\=rs2, T\=rs3, T\=rs4, T\=rs5.
possible(Board, Coord, o):-element(Board, (_,T), Coord), T\=oka, T\=os1, T\=os2, T\=os3, T\=os4, T\=os5.

typeCase(Board, (L,C), Type):-element(Board, (Type,_), (L,C)).

except([], _, []):-!.
except([E|T], D, R):-present(D, E), !, except(T, D, R).
except([H|T], D, [H|R]):-except(T, D, R).

pionDuJoueur(rka, r).
pionDuJoueur(rs1, r).
pionDuJoueur(rs2, r).
pionDuJoueur(rs3, r).
pionDuJoueur(rs4, r).
pionDuJoueur(rs5, r).

pionDuJoueur(oka, o).
pionDuJoueur(os1, o).
pionDuJoueur(os2, o).
pionDuJoueur(os3, o).
pionDuJoueur(os4, o).
pionDuJoueur(os5, o).

pionsLine([], _, [], _).
pionsLine([(_,P)|Q], J, [P|R], (L,C)):-pionDuJoueur(P,J), Tmp is C+1, pionsLine(Q, J, R, (L, Tmp)),!.
pionsLine([_|Q], J, R, (L,C)):-Tmp is C+1, pionsLine(Q, J, R, (L, Tmp)),!.
pions([], _, [], _).
pions([T|Q], J, R, (L,C)):-Tmp is L+1, pionsLine(T, J, R1, (L,C)), pions(Q, J, R2, (Tmp,C)), concat(R1, R2, R).

pionsPerdu(Board, r, Res):-pions(Board, r, R, (1,1)), except([rka, rs1, rs2, rs3, rs4, rs5], R, Res).
pionsPerdu(Board, o, Res):-pions(Board, o, R, (1,1)), except([oka, os1, os2, os3, os4, os5], R, Res).

estPresent([T|_], T):-!.
estPresent([_|Q], X):-estPresent(Q, X).

perdu(Board, r):-pionsPerdu(Board, r, Tmp), estPresent(Tmp, rka).
perdu(Board, o):-pionsPerdu(Board, o, Tmp), estPresent(Tmp, oka).

casePionsLine([], _, [], _).
casePionsLine([(T,P)|Q], J, [(T,(L,C))|R], (L,C)):-pionDuJoueur(P,J), Tmp is C+1, casePionsLine(Q, J, R, (L, Tmp)),!.
casePionsLine([_|Q], J, R, (L,C)):-Tmp is C+1, casePionsLine(Q,J,R, (L, Tmp)).

casePions([], _, [], _).
casePions([T|Q], J, R, (L,C)):-Tmp is L+1, casePionsLine(T, J, R1, (L,C)), casePions(Q, J, R2, (Tmp,C)), concat(R1, R2, R).

triPions([], []).
triPions([(_,P)|Q], [P|R]):-triPions(Q, R).
triPionsKhan([], _, []).
triPionsKhan([(K,P)|Q], K, [P|R]):-triPionsKhan(Q, K, R),!.
triPionsKhan([(_,_)|Q], K, R):-triPionsKhan(Q, K, R).

pions(Board, J, R):-casePions(Board, J, Tmp, (1,1)), triPions(Tmp, R).
pionsKhan(Board, J, K, R):-casePions(Board, J, Tmp, (1,1)), triPionsKhan(Tmp, K, R).

proche((L,C1),(L,C2)):-C2 is C1+1.
proche((L,C1),(L,C2)):-C2 is C1-1.
proche((L1,C),(L2,C)):-L2 is L1+1.
proche((L1,C),(L2,C)):-L2 is L1-1.

coordIdentique((A,B),(A,B)).

possibleMove(Board, CoordPion, From, To, 1, Joueur):-proche(CoordPion, To), possible(Board, To, Joueur), \+ coordIdentique(From, To).
possibleMove(Board, CoordPion, From, To, N, Joueur):-N\=1, possibleMove(Board, CoordPion, From, NewCoord, 1, Joueur), libre(Board, NewCoord), Tmp is N-1, possibleMove(Board, NewCoord, CoordPion, To, Tmp, Joueur).

possibleMovePion(Board, CoordPion, Mouvements, Joueur):-typeCase(Board, CoordPion, Type), setof(To, possibleMove(Board, CoordPion, CoordPion, To, Type, Joueur), Mouvements).

possibleMovePions(_, [], [], _):-!.
possibleMovePions(Board, [P1|P2], [(P1,R1)|R2], Joueur):-possibleMovePion(Board, P1, R1, Joueur), possibleMovePions(Board, P2, R2, Joueur),!.
possibleMovePions(Board, [P1|P2], R, Joueur):-possibleMovePion(Board, P1, [], Joueur), possibleMovePions(Board, P2, R, Joueur).

possibleMoves(Board, Joueur, Mouvements, n):-
	pions(Board, Joueur, Pions),
	write(' ---- MvtP '), write(Pions), write(' ----'), nl,
	possibleMovePions(Board, Pions, Mouvements, Joueur),
	write(' ---- MvtP '), write(Mouvements), write(' ----'), nl, !.

possibleMoves(Board, Joueur, Mouvements, Khan):-
	element(Board, (Type, _), Khan),
	write(' --> Le Khan est en position '), write(Khan), write(' sur une case de type '), write(Type), nl,
	pionsKhan(Board, Joueur, Type, Pions),
	write(' ---- MvtKhan '), write(Pions), write(' ----'), nl,
	possibleMovePions(Board, Pions, Mouvements, Joueur),
	write(' ---- MvtKhan '), write(Mouvements), write(' ----'), nl.

choix(S, N, Min, Max):-
	repeat,
	write(S),
	read(Tmp),
	Tmp>=Min,
	Tmp=<Max,
	N is Tmp.

choixMouvement(T, Coord, Mouvement):-
	taille(Mouvement, T1),
	choix('Quel piece voulez vous deplacer ? ', N, 1, T1),
	element_colonne(Mouvement, (T,P), N),
	taille(P, T2),
	affichePossibleMovesPiece(P, 1),
	choix('En quel position ? ', M, 1, T2),
	element_colonne(P, Coord, M).

deplacer(Depart, Arrive, Board, Res):-
	element(Board, (_, E), Depart),
	retirer_pion(Board, B1, Depart),
	ajouter_pion(B1, Res, E, Arrive).

saisiePionPerdu(Board, Pions, Res):-
	repeat,
	taille(Pions, Taille),
	write('Votre choix : '),
	read(Choix),
	Choix>=1, Choix=<Taille,
	element_colonne(Pions, Pion, Choix),
	write('Entrez une ligne : '),
	read(L),
	L>=1, L=<6,
	write('Entrez une colonne : '),
	read(C),
	C>=1, C=<6,
	ajouter_pion_libre(Board, Res, Pion, (L,C)), write('  TEST PION   '), nl, !.

tourJoueurSansKhan(Board, Res, Joueur, C, _, Coord):-
	C is 1,
	possibleMoves(Board, Joueur, Mouvement, n),
	affichePossibleMoves(Mouvement, 1),
	choixMouvement(Depart, Coord, Mouvement),
	deplacer(Depart, Coord, Board, Res).

tourJoueurSansKhan(Board, Res, Joueur, C, Khan, Khan):-
	C is 2,
	pionsPerdu(Board, Joueur, Pions),
	affichePossibleMovesPiece(Pions, 1),
	saisiePionPerdu(Board, Pions, Res),!.

tourJoueur(Board, Res, Joueur, Khan, NewKhan):-
	write('  TEST 1  '), write(Khan), nl,
	possibleMoves(Board, Joueur, [], Khan),
	write('  TEST 1 - 2 '), nl,
	pionsPerdu(Board, Joueur, []),
	write('  TEST 1 - 3 '), nl,
	tourJoueurSansKhan(Board, Res, Joueur, 1, Khan, NewKhan),!.

tourJoueur(Board, Res, Joueur, Khan, NewKhan):-
	write('  TEST 2  '), nl,
	possibleMoves(Board, Joueur, [], Khan),
	write('  TEST 2 - 2  '), nl,
	repeat,
	write('1 pour deplacer une piece, 2 pour ajouter une piece perdue : '),
	read(C),
	C>=1, C=<2,
	tourJoueurSansKhan(Board, Res, Joueur, C, Khan, NewKhan),!.

tourJoueur(Board, Res, Joueur, Khan, Coord):-
	write('  TEST 3  '), nl,
	possibleMoves(Board, Joueur, Mouvement, Khan),
	write('  TEST 3 - 2  '), nl,
	affichePossibleMoves(Mouvement, 1),
	choixMouvement(Depart, Coord, Mouvement),
	deplacer(Depart, Coord, Board, Res).

gagne(Board):-
	perdu(Board, o),
	write('Rouge a gagné !').

gagne(Board):-
	perdu(Board, r),
	write('Ocre a gagné !').

tours(Board, _, _):-gagne(Board),!.

tours(Board, r, Khan):-
	write('Au tour de rouge.'), nl,
	tourJoueur(Board, R, r, Khan, NewKhan),
	write(NewKhan), nl,
	affiche(R),
	tours(R, o, NewKhan).

tours(Board, o, Khan):-
	write('Au tour de ocre.'), nl,
	tourJoueur(Board, R, o, Khan, NewKhan),
	write(NewKhan), nl,
	affiche(R),
	tours(R, r, NewKhan).

affichePossibleMovesPiece([], _).
affichePossibleMovesPiece([T|Q], N):-
	write('Choix '), write(N), write(' : '), write(T),
	Tmp is N+1,
	nl,
	affichePossibleMovesPiece(Q, Tmp).

affichePossibleMoves([(T,P)], N):-write('Choix '), write(N), write(' : '), write(T), write(' -> '), write(P), nl,!.
affichePossibleMoves([(T,P)|Q], N):-
	write('Choix '), write(N), write(' : '), write(T), write(' -> '), write(P), nl,
	Tmp is N+1,
	affichePossibleMoves(Q, Tmp).
