% Reprezentacja programu:
% state(lista  val(zmienna, wartość), lista val(nazwa tablicy, lista kolejnych wartości), lista z licznikiem instrukcji)

vars([k]).
arrays([chce]).
program1([assign(arr(chce, pid), 1), assign(k, pid),
	 condGoto(arr(chce, 1-pid) = 0, 5),
	 condGoto(k = pid, 3),
	 sekcja, assign(arr(chce, pid), 0), goto(1)]).

testProgram(program(V, A, 2, P)) :- vars(V), arrays(A), program1(P).

% program(Zmienne, Tablice, N, Cialo)

% initState(Program, StanPoczątkowy).
initState(program(VarsNames, ArraysNames, N, _), state(Vars, Arrays, Ips)) :-
	initVars(VarsNames, Vars),
	initArrays(ArraysNames, N, Arrays),
	initIps(N, Ips).

initVars([], []).
initVars([V|T1], [val(V, 0)|T2]) :-
		initVars(T1, T2).

initArrays([], _, []).
initArrays([V|T1], N, [val(V, Arr)|T2]) :-
	initArray(N, Arr),
	initArrays(T1, N, T2).

initArray(0, []).
initArray(N, [0|T]) :-
	N > 0,
	N1 is N - 1,
	initArray(N1, T).

initIps(N, Ips) :- initArray(N, Ips).

processList(0, []).
processList(N, [N1|T]) :-
	N > 0,
	N1 is N - 1,
	processList(N1, T).

% member(Indeks, Tablica, Wartość).
member(0, [H|_], H). % ew. odcięcie
member(N, [_|T], Res) :-
	N > 0,
	N1 is N - 1,
	member(N1, T, Res).

step(program(_, _, N, S), In, Id, Out) :-
	processList(N, PL),
	member(Id, PL),
	stepAux(S, In, Id, Out).

stepAux(S, state(V1, A1, P1), Id, state(V2, A2, P2)) :-
	member(Id, P1, Cur1), % która instrukcja dla danego procesu
	member(Cur1, S, Stmt),
	stepSingle(Stmt, Id, singleState(V1, A1, Cur1), singleState(V2, A2, Cur2)),
	setPointer(Id, P1, Cur2, P2).

% TODO: rename
setPointer(0, [_|T], New, [New|T]). % ew. odcięcie
setPointer(Id1, [H|T1], New, [H|T2]) :-
	Id1 > 0,
	Id2 is Id1 - 1,
	setPointer(Id2, T1, New, T2).

setVariable([val(X, _)|T], X, N, [val(X, N)|T]). % ew. odcięcie
setVariable([val(Y, V)|T1], X, N, [val(X, V)|T2]) :-
	X \= Y,
	setVariable(T1, X, N, T2).

% setArrayCell(StaraListaTablic, NazwaTablicy, Indeks, NowaWartość, NowaListaTablic).
setArrayCell([val(X, Arr)|T], X, I, N, [val(X, NewArray)|T]) :-
	% ew. odcięcie
	setPointer(I, Arr, N, NewArray). 
setArrayCell([val(Y, V)|T1], X, I, N, [val(X, V)|T2]) :-
	X \= Y,
	setArrayCell(T1, X, I, N, T2).

% pojedyncze kroki dla poszczególnych instrukcji

stepSingle(assign(X, Exp), Id, singleState(V1, A1, P1), singleState(V2, A1, P2)) :-
	member(val(X, _), V1), % do zastanowienia się, ew. różne od arr
	eval(Exp, V1, A1, Id,  N),
	setVariable(V1, X, N, V2),
	P2 is P1 + 1.

stepSingle(assign(arr(X, Exp1), Exp2), Id, singleState(V1, A1, P1), singleState(V1, A2, P2)) :-
	eval(Exp1, V1, A1, Id, I),
	eval(Exp2, V1, A1, Id, N),
	setArrayCell(A1, X, I, N, A2),
	P2 is P1 + 1.

stepSingle(goto(In), _, singleState(V, A, _), singleState(V, A, In)).

stepSingle(sekcja, _, singleState(V, A, P1), singleState(V, A, P2)) :-
	P2 is P1 + 1.

% eval(Wyrażenie, Zmienne, Tablice, NumerProcesu, Wartość).
% TODO: przemyśleć odcięcia
eval(N, _, _, _, N) :- integer(N).
eval(pid, _, _, Id, Id).
eval(arr(V, Exp), Vs, As, Id, N) :-
	member(val(V, Arr), As),
	eval(Exp, Vs, As, Id, I),
	member(I, Arr, N).
eval(V, Vs, _, _, N) :-
	member(val(V, N), Vs). 

% TODO: czy da się ładniej?
eval(E1 + E2, Vs, As, Id, N) :-
	eval(E1, E2, Vs, As, Id, N1, N2),
	N is N1 + N2.
eval(E1 - E2, Vs, As, Id, N) :-
	eval(E1, E2, Vs, As, Id, N1, N2),
	N is N1 - N2.
eval(E1 * E2, Vs, As, Id, N) :-
	eval(E1, E2, Vs, As, Id, N1, N2),
	N is N1 * N2.
eval(E1 / E2, Vs, As, Id, N) :-
	eval(E1, E2, Vs, As, Id, N1, N2),
	N is N1 / N2.

% pomocnicze dla 2 wyrażeń
eval(E1, E2, Vs, As, Id, N1, N2) :-
	eval(E1, Vs, As, Id, N1),
	eval(E2, Vs, As, Id, N2).

:- op(700, xfx, <>).

evalBool(E1 < E2, Vs, As, Id) :-
	eval(E1, E2, Vs, As, Id, N1, N2),
	N1 < N2.

evalBool(E1 = E2, Vs, As, Id) :-
	eval(E1, E2, Vs, As, Id, N1, N2),
	N1 =:= N2.

evalBool(E1 <> E2, Vs, As, Id) :-
	eval(E1, E2, Vs, As, Id, N1, N2),
	N1 =\= N2.

% pomocne w testowaniu
exampleState(P, In) :- testProgram(P), initState(P, In).

testStep(Id, Out) :- exampleState(P, In),
	step(P, In, Id, Out). 
	
