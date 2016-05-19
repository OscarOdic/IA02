initBoard(Board):- Board=[[2,2,3,1,2,2],[1,3,1,3,1,3],[3,1,2,2,3,1],[2,3,1,3,1,2],[2,1,3,1,3,2],[1,3,2,2,1,3]],
					write(`Entrez le nombre de joueurs `), read(X), joue(X).

jeu:-initBoard(Board), affiche(Board).
			
:-dynamic(joueur/1).
joue(0).
joue(1):-assertA(joueur(A)).
joue(2):-assertA(joueur(A)),joueur(B).

concat([], L, L):-!.
concat([T|Q],L,[T|R]):-concat(Q,L,R).

inverse([], []):-!.
inverse([T|Q], B):- inverse(Q,B1), concat(B1, T, B).

oriente(T, T, 1).
oriente([T|Q], Board ,3):- inverse(T,T2), oriente(Q,Q2,3), concat(Q2,T2).

affiche([]):-!.
affiche([T|Q]):-imprime(T), nl, affiche(Q).

imprime([]).
imprime([T|Q]):-write(T), write(' '), imprime(Q).