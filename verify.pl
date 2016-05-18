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
