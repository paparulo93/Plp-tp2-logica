symbol(a).
symbol(b).
symbol(c).

% Algunas regex de ejemplo

regexEj(1, a). % a
regexEj(2, or(a, b)). % a|b
regexEj(3, concat(E1, E2)) :- regexEj(1, E1), regexEj(2, E2). % a(a|b)
regexEj(4, star(E2)) :- regexEj(2, E2). % (a(a|b))*
regexEj(5, or(star(E1), E4)) :- regexEj(1, E1), regexEj(4, E4). % (a*|(a(a|b))*)
regexEj(6, star(or(a, ab))). %(a|ab)*
regexEj(7, concat(or(a, concat(a,b)), or(b, empty))). %(a|ab)(b|)
regexEj(8, concat(star(a), star(b))). % a*b*
regexEj(9, star(or(star(a), star(b)))).


% Ejercicio 1: tieneEstrella(+RegEx)

tieneEstrella(concat(X,_)) :- tieneEstrella(X).
tieneEstrella(concat(_,Y)) :- tieneEstrella(Y).
tieneEstrella(or(X,_)) :- tieneEstrella(X).
tieneEstrella(or(_,Y)) :- tieneEstrella(Y).
tieneEstrella(star(_)).


% Ejercicio 2: longitudMaxima(+RegEx, -Length)
longitudMaxima(empty, 0).
longitudMaxima(X,L) :- symbol(X), L is 1.
longitudMaxima(concat(X,Y), L) :- not(tieneEstrella(concat(X,Y))), longitudMaxima(X, L1), longitudMaxima(Y,L2), L is L1 + L2.
longitudMaxima(or(X,Y), L) :- not(tieneEstrella(or(X,Y))), longitudMaxima(X, L1), longitudMaxima(Y,L2), max_member(L, [L1,L2]).


% Ejercicio 3: cadena(?Cadena)
cadena([]).
cadena([X | XS]):- cadena(XS), symbol(X).

% Ejercicio 4: match_inst(+Cadena, +RegEx)

match_inst([], empty).
match_inst([X], X) :- symbol(X).
match_inst(L, or(X,_)) :- match_inst(L, X).
match_inst(L, or(_,Y)) :- match_inst(L, Y).
match_inst(L, concat(X,Y)) :- append(I, D, L), match_inst(I, X), match_inst(D,Y).
match_inst([], star(_)).
match_inst(L, star(E)) :- append(I, D, L), length(I, T), T > 0, match_inst(I, E), match_inst(D,star(E)).

% Ejercicio 5: match(?Cadena, +RegEx)

match(L, E) :- cadena(L), match_inst(L, E).

% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)

diferencia(L, E1, E2) :- match(L, E1), not(match(L,E2)).

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)

prefijoMaximo(P, L, E) :- append(P,_,L), match(P, E), length(P, T), not(hayPrefijoMayor(L,E,T)).

%hayPrefijoMayor(+L,+E,+T)
hayPrefijoMayor(L, E, T):- append(I,_,L), length(I, TI), TI > T, match(I,E).


% Ejercicio 8: reemplazar(+X, +E, +R, -Res)

reemplazar([], _, _, []).
reemplazar(X, E, R, Res) :- prefijoMaximo(P, X, E), length(P,TamP), TamP > 0, append(P, D, X), reemplazar(D, E, R, ResRec), append(R, ResRec, Res).
reemplazar([X | XS], E, R, [X | Res]) :- prefijoMaximo(P, [X | XS], E), length(P,TamP), TamP =:= 0, reemplazar(XS, E, R, Res).
reemplazar([X | XS], E, R, [X | Res]) :- not(prefijoMaximo(_, [X | XS], E)), reemplazar(XS, E, R, Res).
