% Lance une partie
jeu:-initBoard(Board), affiche(Board), tours(Board, r, n),!.

% Clear de terminal
cls :- write('\33\[2J').

% Plateau par défaut
boardKahn([
[(2,n),(2,n),(3,n),(1,n),(2,n),(2,n)],
[(1,n),(3,n),(1,n),(3,n),(1,n),(3,n)],
[(3,n),(1,n),(2,n),(2,n),(3,n),(1,n)],
[(2,n),(3,n),(1,n),(3,n),(1,n),(2,n)],
[(2,n),(1,n),(3,n),(1,n),(3,n),(2,n)],
[(1,n),(3,n),(2,n),(2,n),(1,n),(3,n)]
]).

% Initialise le plateau avec le nombre de joueurs, l'orientation du plateau,
% et l'ajout des pièces
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

% Defini le nombre de joueurs humains
:-dynamic(joueur/1).
joue(0):-asserta(joueur(r)), asserta(joueur(o)),
	retract(joueur(r)), retract(joueur(o)).
joue(1):-asserta(joueur(r)), asserta(joueur(o)),
	retract(joueur(o)).
joue(2):-asserta(joueur(r)), asserta(joueur(o)).

% Concatene deux listes
concat([], L, L):-!.
concat([T|Q],L,[T|R]):-concat(Q,L,R).

% Inverse une liste
inverse([], []):-!.
inverse([T|Q], B):- inverse(Q,B1), concat(B1, [T], B).

% Oriente le plateau selon un sens, haut, bas, gauche ou droite
oriente([], [], _):-!.
oriente([[]|_],[],_):-!.
oriente(T, T, b).
oriente([T|Q], Board, h):-
	inverse(T,T2),
	oriente(Q,Q2,h),
	concat(Q2,[T2], Board).
oriente(T, Board, g):-
	retireColonne(T,C,Tmp),
	oriente(Tmp, Board2, g),
	concat(Board2,[C],Board).
oriente(T, Board, d):-
	oriente(T,Board2,g),
	oriente(Board2, Board, h).

% Récupere la première colonne d'un plateau et fournis le plateau sans la colonne
retireColonne([],[],[]):-!.
retireColonne([T|Q],C,NewTable):-
	retireColonne(Q,C2,NewTable2),
	premierElement(T,P),
	concat(P,C2,C), finList(T,F), concat([F],NewTable2,NewTable).

% Récupere le premier element d'une liste
premierElement([],[]):-!.
premierElement([T|_],[T]).

% Récupere la fin d'une liste
finList([],[]):-!.
finList([_|Q],Q).

% Affiche une ligne
afficheLine(0):-nl, !.
afficheLine(N):-write(' -------'), N2 is N-1, afficheLine(N2).

% Affiche le plateau de jeu avec les numeros de colonne
affiche(Board):-
	cls,
	write('      1       2       3       4       5       6    '), nl,
	affiche(Board, 1).

% Affiche un plateau
affiche([], _):-write('  '), afficheLine(6), !.
affiche([T|Q], N):-write('  '), afficheLine(6), write(N), write(' | '),
	imprime(T), nl, N2 is N+1, affiche(Q, N2).

% Affiche une liste
imprime([]).
imprime([T|Q]):-imprime_case(T), write(' | '), imprime(Q).

% Affiche une case du plateau
imprime_case((N,n)):-write(N), write('    '),!.
imprime_case((N,T)):-write(N), write(' '), write(T).


/*=============================================================================
Placement_pieces
*/

% L'ajout des pions du joueur rouge humain
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

% L'ajout des pions du joueur rouge IA
ajout_pions(B, Board, r):-
	write('Ajout des pions de l''IA rouge...'), nl,
	ajout_pion_IA(B, B1, rka, r),
	ajout_pion_IA(B1, B2, rs1, r),
	ajout_pion_IA(B2, B3, rs2, r),
	ajout_pion_IA(B3, B4, rs3, r),
	ajout_pion_IA(B4, B5, rs4, r),
	ajout_pion_IA(B5, Board, rs5, r),
	write('Termin�'), nl,
	!.

% L'ajout des pions du joueur ocre humain
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

% L'ajout des pions du joueur ocre IA
ajout_pions(B, Board, o):-
	write('Ajout des pions de l''IA ocre...'), nl,
	ajout_pion_IA(B, B1, oka, o),
	ajout_pion_IA(B1, B2, os1, o),
	ajout_pion_IA(B2, B3, os2, o),
	ajout_pion_IA(B3, B4, os3, o),
	ajout_pion_IA(B4, B5, os4, o),
	ajout_pion_IA(B5, Board, os5, o),
	write('Termin�'), nl,
	!.

% Permet la saisie des coordonnées d'une Kalista pour le joueur rouge humain
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

% Permet la saisie des coordonnées d'un sbire pour le joueur rouge humain
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

% Permet la saisie des coordonnées d'une Kalista pour le joueur ocre humain
saisie_pion(Board, Res, oka, o):-
	repeat,
	write('Entrez la ligne de votre Kalista : '),
	read(L),
	L=<2, L>=1,
	write('Entrez la colonne de votre Kalista : '),
	read(C),
	C=<6, C>=1,
	ajouter_pion_libre(Board, Res, oka, (L,C)),
	affiche(Res).

% Permet la saisie des coordonnées d'un sbire pour le joueur ocre humain
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

% L'ajout d'un pion dans une case libre sur le plateau, suivant des coordonnées
ajouter_pion_libre([T|Q], [Res|Q], X, (1,C)):-ajouter_colonne_init(T,Res,X,C),!.
ajouter_pion_libre([T|Q], [T|R], X, (L,C)):-
	Tmp is L-1, ajouter_pion_libre(Q, R, X, (Tmp,C)).

% L'ajout d'un pion dans une case libre sur une liste, suivant un indice,
% correspondant à la colonne du plateau
ajouter_colonne_init([(N,n)|Q], [(N,X)|Q], X, 1):-!.
ajouter_colonne_init([T|Q], [T|R], X, C):-
	Tmp is C-1, ajouter_colonne_init(Q, R, X, Tmp).

% L'ajout d'un pion dans une case, libre ou non, sur le plateau,
% suivant des coordonnées
ajouter_pion([T|Q], [Res|Q], X, (1,C)):-ajouter_colonne(T,Res,X,C),!.
ajouter_pion([T|Q], [T|R], X, (L,C)):-
	Tmp is L-1, ajouter_pion(Q, R, X, (Tmp,C)).

% L'ajout d'un pion dans une case, libre ou non, sur une liste,
% suivant un indice, correspondant à la colonne du plateau
ajouter_colonne([(N,_)|Q], [(N,X)|Q], X, 1):-!.
ajouter_colonne([T|Q], [T|R], X, C):-Tmp is C-1, ajouter_colonne(Q, R, X, Tmp).

% L'IA rouge saisie son pion
ajout_pion_IA(B, Board, X, r):-
	repeat,
	random(1, 7, C),
	random(5, 7, L),
	ajouter_pion_libre(B, Board, X, (L,C)).

% L'IA ocre saisie son pion
ajout_pion_IA(B, Board, X, o):-
	repeat,
	random(1, 7, C),
	random(1, 3, L),
	ajouter_pion_libre(B, Board, X, (L,C)).

% Retire un pion du plateau
retirer_pion([T|Q], [Res|Q], (1,C)):-retirer_colonne(T,Res,C),!.
retirer_pion([T|Q], [T|R], (L,C)):-Tmp is L-1, retirer_pion(Q, R, (Tmp,C)).

% Retire un pion d'une liste
retirer_colonne([(N,_)|Q], [(N,n)|Q], 1):-!.
retirer_colonne([T|Q], [T|R], C):-Tmp is C-1, retirer_colonne(Q, R, Tmp).

/*=============================================================================
Mouvements possible
*/

% Récupère la taille d'une liste
taille([_], 1):-!.
taille([_|Q], N):-taille(Q, Tmp), N is Tmp + 1.

% Récupère l'element d'un plateau suivant ses coordonnées
element([T|_], Res, (1,C)):-element_colonne(T, Res, C),!.
element([_|Q], Res, (L,C)):-Tmp is L-1, element(Q, Res, (Tmp, C)),!.

% Récupère l'element d'une liste suivant son indice correspondant
% à la colonne d'un plateau
element_colonne([T|_], T, 1):-!.
element_colonne([_|Q], Res, C):-Tmp is C-1, element_colonne(Q, Res, Tmp).

% Si un element est présent dans une liste
present([T|_], T):-!.
present([_|Q], D):-present(Q, D).

% Si la case des coordonnées du plateau est une case libre (sans pièce)
libre(Board, Coord):-element(Board, (_,n), Coord).

% Si le mouvement est possible, les coordonnées donnés pointe vers une case qui,
% soit est libre, soit on y trouve une pièce adverse
possible(Board, Coord, r):-
	element(Board, (_,T), Coord), T\=rka, T\=rs1, T\=rs2, T\=rs3, T\=rs4, T\=rs5.
possible(Board, Coord, o):-
	element(Board, (_,T), Coord), T\=oka, T\=os1, T\=os2, T\=os3, T\=os4, T\=os5.

% Récupère le type de case (1,2 ou 3)
typeCase(Board, (L,C), Type):-element(Board, (Type,_), (L,C)).

% Retire d'une liste, les elements d'une autre liste
except([], _, []):-!.
except([E|T], D, R):-present(D, E), !, except(T, D, R).
except([H|T], D, [H|R]):-except(T, D, R).

% Définie les pions du joueur rouge
pionDuJoueur(rka, r).
pionDuJoueur(rs1, r).
pionDuJoueur(rs2, r).
pionDuJoueur(rs3, r).
pionDuJoueur(rs4, r).
pionDuJoueur(rs5, r).

% Définie les pions du joueur ocre
pionDuJoueur(oka, o).
pionDuJoueur(os1, o).
pionDuJoueur(os2, o).
pionDuJoueur(os3, o).
pionDuJoueur(os4, o).
pionDuJoueur(os5, o).

% Récupere les noms des pièces du joueur encore en jeu
pionsLine([], _, [], _).
pionsLine([(_,P)|Q], J, [P|R], (L,C)):-
	pionDuJoueur(P,J), Tmp is C+1, pionsLine(Q, J, R, (L, Tmp)),!.
pionsLine([_|Q], J, R, (L,C)):-Tmp is C+1, pionsLine(Q, J, R, (L, Tmp)),!.
pions([], _, [], _).
pions([T|Q], J, R, (L,C)):-
	Tmp is L+1, pionsLine(T, J, R1, (L,C)),
	pions(Q, J, R2, (Tmp,C)), concat(R1, R2, R).

% Récupère les pions perdu du joueur
pionsPerdu(Board, r, Res):-
	pions(Board, r, R, (1,1)), except([rka, rs1, rs2, rs3, rs4, rs5], R, Res).
pionsPerdu(Board, o, Res):-
	pions(Board, o, R, (1,1)), except([oka, os1, os2, os3, os4, os5], R, Res).

% Si le joueur a perdu
perdu(Board, r):-pionsPerdu(Board, r, Tmp), present(Tmp, rka).
perdu(Board, o):-pionsPerdu(Board, o, Tmp), present(Tmp, oka).

% Récupère les cases des pièces du joueur encore en jeu ainsi que
% les types des cases
casePionsLine([], _, [], _).
casePionsLine([(T,P)|Q], J, [(T,(L,C))|R], (L,C)):-
	pionDuJoueur(P,J), Tmp is C+1, casePionsLine(Q, J, R, (L, Tmp)),!.
casePionsLine([_|Q], J, R, (L,C)):-Tmp is C+1, casePionsLine(Q,J,R, (L, Tmp)).
casePions([], _, [], _).
casePions([T|Q], J, R, (L,C)):-
	Tmp is L+1, casePionsLine(T, J, R1, (L,C)),
	casePions(Q, J, R2, (Tmp,C)), concat(R1, R2, R).

% Filtre une liste généré par le predicat precedent afin de ne garder que
% les coordonnées (en fonction du Khan ou non)
triPions([], []).
triPions([(_,P)|Q], [P|R]):-triPions(Q, R).
triPionsKhan([], _, []).
triPionsKhan([(K,P)|Q], K, [P|R]):-triPionsKhan(Q, K, R),!.
triPionsKhan([(_,_)|Q], K, R):-triPionsKhan(Q, K, R).

% Récupère les cases des pièces du joueur qu'il peut jouer en fonction du Khan
pions(Board, J, R):-casePions(Board, J, Tmp, (1,1)), triPions(Tmp, R).
pionsKhan(Board, J, K, R):-
	casePions(Board, J, Tmp, (1,1)), triPionsKhan(Tmp, K, R).

% Si les deux cases sont proches en fonction de leurs coordonnées
proche((L,C1),(L,C2)):-C2 is C1+1.
proche((L,C1),(L,C2)):-C2 is C1-1.
proche((L1,C),(L2,C)):-L2 is L1+1.
proche((L1,C),(L2,C)):-L2 is L1-1.

% Si les coodonnées sont identiques
coordIdentique((A,B),(A,B)).

% Récupère un mouvement possible d'une pièce
possibleMove(Board, CoordPion, From, To, 1, Joueur):-
	proche(CoordPion, To), possible(Board, To, Joueur),
	\+ coordIdentique(From, To).
possibleMove(Board, CoordPion, From, To, N, Joueur):-
	N\=1, possibleMove(Board, CoordPion, From, NewCoord, 1, Joueur),
	libre(Board, NewCoord), Tmp is N-1,
	possibleMove(Board, NewCoord, CoordPion, To, Tmp, Joueur).

% Récupère tous les mouvements possible d'une pièce
possibleMovePion(Board, CoordPion, Mouvements, Joueur):-
	typeCase(Board, CoordPion, Type),
	setof(To, possibleMove(Board, CoordPion, CoordPion, To, Type, Joueur), Mouvements).

% Récupère tous les mouvements possibles d'une liste de pions
possibleMovePions(_, [], [], _):-!.
possibleMovePions(Board, [P1|P2], [(P1,R1)|R2], Joueur):-
	possibleMovePion(Board, P1, R1, Joueur),
	possibleMovePions(Board, P2, R2, Joueur),!.
possibleMovePions(Board, [P1|P2], R, Joueur):-
	\+ possibleMovePion(Board, P1, _, Joueur),
	possibleMovePions(Board, P2, R, Joueur).

% Récupère tous les mouvements possibles d'un joueur sans prendre
% en compte le khan
possibleMoves(Board, Joueur, Mouvements, n):-
	pions(Board, Joueur, Pions),
	possibleMovePions(Board, Pions, Mouvements, Joueur),!.

% Récupère tous les mouvements possibles d'un joueur en prennant
% en compte le khan
possibleMoves(Board, Joueur, Mouvements, Khan):-
	element(Board, (Type, _), Khan),
	pionsKhan(Board, Joueur, Type, Pions),
	possibleMovePions(Board, Pions, Mouvements, Joueur).

/*=============================================================================
Tours de jeu humain
*/

% Permet à l'utilisateur de choisir une valeur entre Min et Max,
% se repete tant qu'il choisit une valeur incorrecte
choix(S, N, Min, Max):-
	repeat,
	write(S),
	read(Tmp),
	Tmp>=Min,
	Tmp=<Max,
	N is Tmp.

% Permet de choisir un mouvement parmis une liste de mouvements par pièce
choixMouvement(T, Coord, Mouvement):-
	taille(Mouvement, T1),
	choix('Quel piece voulez vous deplacer ? ', N, 1, T1),
	element_colonne(Mouvement, (T,P), N),
	taille(P, T2),
	affichePossibleMovesPiece(P, 1),
	choix('En quel position ? ', M, 1, T2),
	element_colonne(P, Coord, M).

% Déplace un pion sur le plateau
deplacer(Depart, Arrive, Board, Res):-
	element(Board, (_, E), Depart),
	retirer_pion(Board, B1, Depart),
	ajouter_pion(B1, Res, E, Arrive).

% Permet à un joueur de saisir un point perdu sur le plateau
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
	ajouter_pion_libre(Board, Res, Pion, (L,C)),!.

% Le tour d'un joueur humain, pour le déplacement d'une pièce,
% sans prendre en compte le Khan
tourJoueurSansKhan(Board, Res, Joueur, C, _, Coord):-
	C is 1,
	possibleMoves(Board, Joueur, Mouvement, n),
	affichePossibleMoves(Mouvement, 1),
	choixMouvement(Depart, Coord, Mouvement),
	deplacer(Depart, Coord, Board, Res).

% Le tour d'un joueur humain, pour l'ajout d'une pièce perdu
tourJoueurSansKhan(Board, Res, Joueur, C, Khan, Khan):-
	C is 2,
	pionsPerdu(Board, Joueur, Pions),
	affichePossibleMovesPiece(Pions, 1),
	saisiePionPerdu(Board, Pions, Res),!.

% Le tour d'un joueur humain, ou aucun mouvement n'est possible à cause du Khan,
% et où aucun pions n'a été perdu
tourJoueur(Board, Res, Joueur, Khan, NewKhan):-
	possibleMoves(Board, Joueur, [], Khan),
	pionsPerdu(Board, Joueur, []),
	tourJoueurSansKhan(Board, Res, Joueur, 1, Khan, NewKhan),!.

% Le tour d'un joueur humain, ou aucun mouvement n'est possible à cause du Khan,
% mais où au moins un pion a été perdu
tourJoueur(Board, Res, Joueur, Khan, NewKhan):-
	possibleMoves(Board, Joueur, [], Khan),
	repeat,
	write('1 pour deplacer une piece, 2 pour ajouter une piece perdue : '),
	read(C),
	C>=1, C=<2,
	tourJoueurSansKhan(Board, Res, Joueur, C, Khan, NewKhan),!.

% Tour d'un joueur avec mouvements possibles en fonction du Khan
tourJoueur(Board, Res, Joueur, Khan, Coord):-
	possibleMoves(Board, Joueur, Mouvement, Khan),
	affichePossibleMoves(Mouvement, 1),
	choixMouvement(Depart, Coord, Mouvement),
	deplacer(Depart, Coord, Board, Res).

% Affiches toues les mouvements possible d'une pièce
affichePossibleMovesPiece([], _).
affichePossibleMovesPiece([T|Q], N):-
	write('Choix '), write(N), write(' : '), write(T),
	Tmp is N+1,
	nl,
	affichePossibleMovesPiece(Q, Tmp).

% Affiches tous les mouvements possibles
affichePossibleMoves([(T,P)], N):-
	write('Choix '), write(N), write(' : '),
	write(T), write(' -> '), write(P), nl,!.
affichePossibleMoves([(T,P)|Q], N):-
	write('Choix '), write(N), write(' : '),
	write(T), write(' -> '), write(P), nl,
	Tmp is N+1,
	affichePossibleMoves(Q, Tmp).

/*=============================================================================
Tours de jeu
*/

% Le joueur ocre a gagné
gagne(Board):-
	perdu(Board, o),
	write('Rouge a gagn� !').

% Le joueur rouge a gagné
gagne(Board):-
	perdu(Board, r),
	write('Ocre a gagn� !').

% Fin du jeu
tours(Board, _, _):-gagne(Board),!.

% Tour du joueur humain rouge
tours(Board, r, Khan):-
	joueur(r),
	write('Au tour de rouge.'), nl,
	tourJoueur(Board, R, r, Khan, NewKhan),
	affiche(R),
	write(' --> Le Khan est en position '), write(NewKhan), nl,
	tours(R, o, NewKhan),!.

% Tour du joueur humain ocre
tours(Board, o, Khan):-
	joueur(o),
	write('Au tour de ocre.'), nl,
	tourJoueur(Board, R, o, Khan, NewKhan),
	affiche(R),
	write(' --> Le Khan est en position '), write(NewKhan), nl,
	tours(R, r, NewKhan),!.

% Tour du joueur IA rouge
tours(Board, r, Khan):-
	write('Au tour de l\'ia rouge'), nl,
	sleep(0.5),
	tourIa(Board, R, r, Khan, NewKhan),
	affiche(R),
	write(' --> Le Khan est en position '), write(NewKhan), nl,
	tours(R, o, NewKhan),!.

% Tour du joueur IA ocre
tours(Board, o, Khan):-
	write('Au tour de l\'ia ocre'), nl,
	sleep(0.5),
	tourIa(Board, R, o, Khan, NewKhan),
	affiche(R),
	write(' --> Le Khan est en position '), write(NewKhan), nl,
	tours(R, r, NewKhan),!.

/*=============================================================================
Mouvements IA
*/

% Recupere a partir d'une liste de mouvements uniquement les positions possibles
%  d'arrivés (sans les positions de départ des pièces)
listeMouvements([], []):-!.
listeMouvements([(_,M)|Q], Res):- listeMouvements(Q, R), concat(M, R, Res).

% à partir d'une liste de coordonnées, vrai si une Kalista s'y trouve
coordKalista(Board, [T|_]):- element(Board, (_,rka), T),!.
coordKalista(Board, [T|_]):- element(Board, (_,oka), T),!.
coordKalista(Board, [_|Q]):- coordKalista(Board, Q).

% Récupere les mouvement possible de la kalista à partir
% d'une liste de mouvements
mouvementsKalista(_, [], []):-!.
mouvementsKalista(Board, [(P,M)|_], M):-element(Board, (_,rka), P),!.
mouvementsKalista(Board, [(P,M)|_], M):-element(Board, (_,oka), P),!.
mouvementsKalista(Board, [_|Q], Res):-mouvementsKalista(Board, Q, Res).

% Récupère l'ensemble des mouvements disponibles, si aucun n'est disponible
% à cause du Khan, récupère tous les mouvements de toutes les pièces
mouvementsPossiblesKhan(Board, Joueur, Mouvement, Khan):-
	possibleMoves(Board, Joueur, [], Khan),
	possibleMoves(Board, Joueur, Mouvement, n),!.
mouvementsPossiblesKhan(Board, Joueur, Mouvement, Khan):- possibleMoves(Board, Joueur, Mouvement, Khan).

% Si l'un des joueur a perdu, l'evaluation donne un cout très grand
evaluer(Board, r, _, 10000):-
	perdu(Board, o),!.
evaluer(Board, r, _, -10000):-
	perdu(Board, r),!.
evaluer(Board, o, _, 10000):-
	perdu(Board, r),!.
evaluer(Board, o, _, -10000):-
	perdu(Board, o),!.

% Si aucun joueur n'a perdu, procède à la somme des 4 évaluations suivantes :
% Mouvements Kalista, Mouvement Kalista adverse, Mouvement Pièces, Mouvement Pièces adverses
evaluer(Board, r, Khan, Cout):-
	mouvementsPossiblesKhan(Board, r, MouvementsRouge, Khan),
	mouvementsPossiblesKhan(Board, o, MouvementsOcre, Khan),
	evaluer_Mvt_Kalista(Board, MouvementsRouge, Cout1),
	evaluer_Mvt_Kalista_adverse(Board, MouvementsOcre, Cout2),
	evaluer_Mvt(MouvementsRouge, Cout3),
	evaluer_Mvt_adverse(MouvementsOcre, Cout4),
	Cout is Cout1 + Cout2 + Cout3 + Cout4,!.
evaluer(Board, o, Khan, Cout):-
	mouvementsPossiblesKhan(Board, o, MouvementsRouge, Khan),
	mouvementsPossiblesKhan(Board, r, MouvementsOcre, Khan),
	evaluer_Mvt_Kalista(Board, MouvementsOcre, Cout1),
	evaluer_Mvt_Kalista_adverse(Board, MouvementsRouge, Cout2),
	evaluer_Mvt(MouvementsOcre, Cout3),
	evaluer_Mvt_adverse(MouvementsRouge, Cout4),
	Cout is Cout1 + Cout2 + Cout3 + Cout4,!.

% Si aucun joueur n'a perdu, le cout est de 10 fois le nombre de
% mouvements possibles par sa kalista
evaluer_Mvt_Kalista(Board, Mouvement, 0):-
	mouvementsKalista(Board, Mouvement, []),!.
evaluer_Mvt_Kalista(Board, Mouvement, Cout):-
	mouvementsKalista(Board, Mouvement, Kalista),
	taille(Kalista, T), Cout is T*10,!.

% Si aucun joueur n'a perdu, le cout est de -5 fois le nombre de
% mouvements possibles par la kalista adverse
evaluer_Mvt_Kalista_adverse(Board, Mouvement, 0):-
	mouvementsKalista(Board, Mouvement, []),!.
evaluer_Mvt_Kalista_adverse(Board, Mouvement, Cout):-
	mouvementsKalista(Board, Mouvement, Kalista),
	taille(Kalista, T), Cout is -T*5,!.

% si aucun joueur n'a perdu et la kalista est immobile, le cout est
% le nombre de deplacements possibles
evaluer_Mvt(Mouvement, Cout):-
	listeMouvements(Mouvement, Liste),
	taille(Liste, Cout).

% si aucun joueur n'a perdu et la kalista est immobile, le cout est
% le nombre de deplacements possibles
evaluer_Mvt_adverse(Mouvement, Cout):-
	listeMouvements(Mouvement, Liste),
	taille(Liste, Tmp), Cout is -Tmp/2.

% Récupère le plus grand nombre entre deux nombres
majMax(Val1, Val2, Val1):-Val1>=Val2,!.
majMax(_, V, V).

% Récupère le plus petit nombre entre deux nombres
majMin(Val1, Val2, Val1):-Val1=<Val2,!.
majMin(_, V, V).

		/*===================== MAX =====================*/

% Fonction max de l'algorithme minMax : s'arrête si la profondeur
% est null ou negative
max(Board, Joueur, Khan, P, Max):-
	P=<0,
	evaluer(Board, Joueur, Khan, Max),!.

% Fonction max de l'algorithme minMax : s'arrête si une kalista est perdue
max(Board, Joueur, Khan, _, Max):-
	perdu(Board, _),
	evaluer(Board, Joueur, Khan, Max),!.

% Fonction max de l'algorithme minMax : Si aucun mouvement n'est possible,
% on execute la boucle de la fonction max sont khan
max(Board, Joueur, Khan, Profondeur, Max):-
	possibleMoves(Board, Joueur, [], Khan),
	possibleMoves(Board, Joueur, Mouvement, n),
	maxBoucle(Board, Mouvement, Joueur, n, -10000, Max, Profondeur),!.

% Fonction max de l'algorithme minMax : On execute la boucle avec khan
max(Board, Joueur, Khan, Profondeur, Max):-
	possibleMoves(Board, Joueur, Mouvement, Khan),
	maxBoucle(Board, Mouvement, Joueur, Khan, -10000, Max, Profondeur),!.

% La boucle principale de la fonction max, sur chaque pion
maxBoucle(_, [], _, _, Max, Max, _):-!.
maxBoucle(Board, [T|Q], Joueur, Khan, Max, NewMax, Profondeur):-
	maxBouclePion(Board, T, Joueur, Khan, Max, Tmp, Profondeur),
	maxBoucle(Board, Q, Joueur, Khan, Tmp, NewMax, Profondeur),!.

% La boucle pour chaque mouvements possibles d'un pion
maxBouclePion(_, (_, []), _, _, Max, Max, _):-!.
maxBouclePion(Board, (Debut, [T|Q]), r, Khan, Max, NewMax, Profondeur):-
	deplacer(Debut, T, Board, R),
	P is Profondeur-1,
	min(R, o, T, P, Min),
	majMax(Min, Max, Tmp),
	maxBouclePion(Board, (Debut, Q), r, Khan, Tmp, NewMax, Profondeur),!.
maxBouclePion(Board, (Debut, [T|Q]), o, Khan, Max, NewMax, Profondeur):-
	deplacer(Debut, T, Board, R),
	P is Profondeur-1,
	min(R, r, T, P, Min),
	majMax(Min, Max, Tmp),
	maxBouclePion(Board, (Debut, Q), o, Khan, Tmp, NewMax, Profondeur),!.

		/*===================== Min =====================*/

% Fonction min de l'algorithme minMax : s'arrête si la profondeur
% est null ou negative
min(Board, Joueur, Khan, P, Min):-
	P=<0,
	evaluer(Board, Joueur, Khan, Min),!.

% Fonction min de l'algorithme minMax : s'arrête si une kalista est perdue
min(Board, Joueur, Khan, _, Min):-
	perdu(Board, _),
	evaluer(Board, Joueur, Khan, Min),!.

% Fonction min de l'algorithme minMax : Si aucun mouvement n'est possible,
% on execute la boucle de la fonction max sont khan
min(Board, Joueur, Khan, Profondeur, Min):-
	possibleMoves(Board, Joueur, [], Khan),
	possibleMoves(Board, Joueur, Mouvement, n),
	minBoucle(Board, Mouvement, Joueur, n, 10000, Min, Profondeur),!.

% Fonction min de l'algorithme minMax : On execute la boucle avec khan
min(Board, Joueur, Khan, Profondeur, Min):-
	possibleMoves(Board, Joueur, Mouvement, Khan),
	minBoucle(Board, Mouvement, Joueur, Khan, 10000, Min, Profondeur),!.

% La boucle principale de la fonction min, sur chaque pion
minBoucle(_, [], _, _, Min, Min, _):-!.
minBoucle(Board, [T|Q], Joueur, Khan, Min, NewMin, Profondeur):-
	minBouclePion(Board, T, Joueur, Khan, Min, Tmp, Profondeur),
	minBoucle(Board, Q, Joueur, Khan, Tmp, NewMin, Profondeur),!.

% La boucle pour chaque mouvements possibles d'un pion
minBouclePion(_, (_, []), _, _, Min, Min, _):-!.
minBouclePion(Board, (Debut, [T|Q]), r, Khan, Min, NewMin, Profondeur):-
	deplacer(Debut, T, Board, R),
	P is Profondeur-1,
	max(R, o, T, P, Max),
	majMin(Min, Max, Tmp),
	minBouclePion(Board, (Debut, Q), r, Khan, Tmp, NewMin, Profondeur),!.
minBouclePion(Board, (Debut, [T|Q]), o, Khan, Min, NewMin, Profondeur):-
	deplacer(Debut, T, Board, R),
	P is Profondeur-1,
	max(R, r, T, P, Max),
	majMin(Min, Max, Tmp),
	minBouclePion(Board, (Debut, Q), o, Khan, Tmp, NewMin, Profondeur),!.

		/*===================== TOUR =====================*/

% La liste des coups d'un joueur, avec pour chacune, son depart,
% son arrivée et son evaluation
listeCoupsEval(_, _, [], _, [], _):-!.
listeCoupsEval(Board, Joueur, [(P,M)|Q], Khan, Coups, Profondeur):-
	listeCoupsEvalPion(Board, (P,M), Joueur, Khan, Profondeur, [], Liste),
	listeCoupsEval(Board, Joueur, Q, Khan, CoupsTmp, Profondeur),
	concat(Liste, CoupsTmp, Coups).

% la liste des coups d'un pion évalués
listeCoupsEvalPion(_, (_, []), _, _, _, Liste, Liste):-!.
listeCoupsEvalPion(Board, (Debut, [T|Q]), Joueur, Khan, Profondeur, Liste, NewListe):-
	deplacer(Debut, T, Board, R),
	min(R, Joueur, Khan, Profondeur, Eval),
	concat(Liste, [((Debut, T), Eval)], Tmp),
	listeCoupsEvalPion(Board, (Debut, Q), Joueur, Khan, Profondeur, Tmp, NewListe).

% A partir de la liste des coups évalués, récupère le coup avec
% la plus grande évaluation
meilleurCoup([T], T):-!.
meilleurCoup([(_,Eval)|Q], (MeilleurCoup, EvalMeilleurCoup)):-
	meilleurCoup(Q, (MeilleurCoup,EvalMeilleurCoup)),
	EvalMeilleurCoup>=Eval.
meilleurCoup([(Coup,Eval)|Q], (Coup, Eval)):-
	meilleurCoup(Q, (_,EvalTmp)),
	Eval>=EvalTmp.

executerAjoutOuCoup(((Debut, Arrive), EvalCoup), (_, EvalAjout), Board, Res, _, Arrive):-
	EvalCoup>=EvalAjout,
	deplacer(Debut, Arrive, Board, Res), !.

executerAjoutOuCoup(_, ((Pion, Coord), _), Board, Res, Khan, Khan):-
	ajouter_pion_libre(Board, Res, Pion, Coord).

% Tour d'un joueur IA avec aucun mouvement possible suite à la valeur du khan,
% et aucun pions perdu a ajouté sur le plateau
tourIa(Board, Res, Joueur, Khan, Arrive):-
	possibleMoves(Board, Joueur, [], Khan),
	possibleMoves(Board, Joueur, Mouvements, n),
	listeCoupsEval(Board, Joueur, Mouvements, n, Coups, 1),
	meilleurCoup(Coups, ((Debut, Arrive), _)),
	deplacer(Debut, Arrive, Board, Res), !.

% Tour d'un joueur IA
tourIa(Board, Res, Joueur, Khan, Arrive):-
	possibleMoves(Board, Joueur, Mouvements, Khan),
	listeCoupsEval(Board, Joueur, Mouvements, Khan, Coups, 1),
	meilleurCoup(Coups, ((Debut, Arrive), _)),
	deplacer(Debut, Arrive, Board, Res).
