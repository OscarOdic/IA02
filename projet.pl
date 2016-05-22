boardKahn([[2,2,3,1,2,2],[1,3,1,3,1,3],[3,1,2,2,3,1],[2,3,1,3,1,2],[2,1,3,1,3,2],[1,3,2,2,1,3]]).

initBoard(Board):- boardKahn(TmpBoard),
		write(`Entrez le nombre de joueurs : `),
		read(X),
		joue(X),
		affiche(TmpBoard),
		write(`Joueur 1, entrez une orientation[h/b/g/d] : `),
		read(Y),
		oriente(TmpBoard, Board, Y),
		affiche(Board),
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
imprime([T|Q]):-write(T), write(' | '), imprime(Q).
