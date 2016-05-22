% Aleksander Matusiak

:- ensure_loaded(library(lists)).
:- op(700, xfx, <>). % konieczna definicja operatora różności

% Reprezentacja programu:

% state(ZmienneProste, Tablice, LicznikiRozkazu) -
% reprezentacja stanu systemu
% ZmienneProste = [val(Zmienna, Wartość)],
% Tablice = [val(NazwaTablicy, [Wartości])],
% LicznikiRozkazu = [LicznikRozkazu] (lista liczb całkowitych,
% numeracja instrukcji od 0 (inaczej niż w opisie)

% singleState(ZmienneProste, Tablice, LicznikRozkazu) - odpowiada
% stanowi pojedynczego procesu - zawiera globalne zmienne i tablice,
% ale tylko licznik rozkazu pojedynczego procesu

% program(Zmienne, Tablice, N, Cialo)

% initState(Program, StanPoczątkowy).
initState(program(VarsNames, ArraysNames, _), N, state(Vars, Arrays, Ips)) :-
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

% step(Program, LiczbaProcesów, StanWe, PrId, StanWy).
% Zmiana w stosunku do specyfikacji!
% Liczba procesów potrzebna jest do generowania możliwych PrId.
step(program(_, _, S), N, In, Id, Out) :-
	processList(N, PL),
	member(Id, PL),
	stepAux(S, In, Id, Out).

stepAux(S, state(V1, A1, P1), Id, state(V2, A2, P2)) :-
	member(Id, P1, Cur1), % która instrukcja dla danego procesu
	member(Cur1, S, Stmt),
	stepSingle(Stmt, Id, singleState(V1, A1, Cur1), singleState(V2, A2, Cur2)),
	setCell(Id, P1, Cur2, P2).

% setCell(Indeks, StaraLista, Wartość, NowaTablica) ==
% w NowaTablicy na indeksie Indeks stoi Wartość, reszta
% indeksów jest niezmieniona (numerujemy od 0)
setCell(0, [_|T], New, [New|T]) :- !.
setCell(Id1, [H|T1], New, [H|T2]) :-
	Id1 > 0,
	Id2 is Id1 - 1,
	setCell(Id2, T1, New, T2).

% setVariable(Lista, Zmienna, Wartość, NowaLista) ==
% W NowaLista zmiennej Zmienna przypisana jest wartość
% Wartość, reszta zmiennych i ich wartości się nie różni
setVariable([val(X, _)|T], X, N, [val(X, N)|T]) :- !.
setVariable([val(Y, V)|T1], X, N, [val(X, V)|T2]) :-
	X \= Y,
	setVariable(T1, X, N, T2).

% setArrayCell(StaraListaTablic, NazwaTablicy, Indeks,
% Wartość, NowaListaTablic) == NowaListaTablic różni
% się od StaraListaTablic tym, że w tablicy NazwaTablicy
% na indeksie Indeks jest wartość Wartość
setArrayCell([val(X, Arr)|T], X, I, N, [val(X, NewArray)|T]) :-
	!,
	setCell(I, Arr, N, NewArray). 
setArrayCell([val(Y, V)|T1], X, I, N, [val(X, V)|T2]) :-
	X \= Y,
	setArrayCell(T1, X, I, N, T2).

% stepSingle(Insrukcja, Proces, StanPojedynczyPoczątkowy,
% StanPojednczyKońcowy) == StanPojedynczyKońcowy odpowiada wykonaniu
% Instrukcji przez Proces w StanPojedynczyPoczątkowy
stepSingle(assign(X, Exp), Id,
	   singleState(V1, A1, P1), singleState(V2, A1, P2)) :-
	X \= arr(_, _),
	eval(Exp, V1, A1, Id,  N),
	setVariable(V1, X, N, V2),
	P2 is P1 + 1.

stepSingle(assign(arr(X, Exp1), Exp2), Id,
	   singleState(V1, A1, P1), singleState(V1, A2, P2)) :-
	eval(Exp1, V1, A1, Id, I),
	eval(Exp2, V1, A1, Id, N),
	setArrayCell(A1, X, I, N, A2),
	P2 is P1 + 1.

stepSingle(goto(In1), _,
	   singleState(V, A, _), singleState(V, A, In)) :-
	In is In1 - 1. % w reprezentacji stanu instrukcje od 0

stepSingle(condGoto(BExp, In1), Id,
	   singleState(V, A, P1), singleState(V, A, P2)) :-
	In is In1 - 1,
	(evalBool(BExp, V, A, Id) ->
	    P2 = In;
	    P2 is P1 + 1
	).

stepSingle(sekcja, _, singleState(V, A, P1), singleState(V, A, P2)) :-
	P2 is P1 + 1.

% eval(Wyrażenie, Zmienne, Tablice, NumerProcesu, WartośćWyrażenia).
eval(N, _, _, _, N) :- integer(N), !.
eval(pid, _, _, Id, Id) :- !.
eval(arr(V, Exp), Vs, As, Id, N) :-
	!,
	member(val(V, Arr), As),
	eval(Exp, Vs, As, Id, I),
	member(I, Arr, N).
% TODO: może poniższe można trochę ładniej
eval(V, Vs, _, _, N) :-
	V \= arr(_, _),
	member(val(V, N), Vs). 

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

% eval(Wyrażenie1, Wyrażenie2, ZmienneProste, Tablie,
% Proces, WartośćWyrażenia1, WartośćWyrażenia2).
eval(E1, E2, Vs, As, Id, N1, N2) :-
	eval(E1, Vs, As, Id, N1),
	eval(E2, Vs, As, Id, N2).

% evalBool(WyrażenieBoolowskie, Zmienne, Tablice, Proces) ==
% WyrażenieBoolowskie jest prawdziwe przy danym wartościowaniu
evalBool(E1 < E2, Vs, As, Id) :-
	eval(E1, E2, Vs, As, Id, N1, N2),
	N1 < N2.
evalBool(E1 = E2, Vs, As, Id) :-
	eval(E1, E2, Vs, As, Id, N1, N2),
	N1 =:= N2.
evalBool(E1 <> E2, Vs, As, Id) :-
	eval(E1, E2, Vs, As, Id, N1, N2),
	N1 =\= N2.

% Sprawdzanie poprawności
% collision(Program, Stan) == w stanie 2 procesy są w sekcji krytycznej
collision(program(_, _, Stmts), state(_, _, Ps), L) :-
	inSection(Ps, Stmts, L),
	length(L, In),
	In > 1.

collision(Program, State) :- collision(Program, State, _).


% inSection(ListaLiczników, TreśćProgramu, ListaProcesówWSekcji).
inSection(Ps, Stmts, In) :- inSection(Ps, Stmts, 0, In).


inSection([], _, _, []).
inSection([H|T], Stmts, I, Res) :-
	(member(H, Stmts, sekcja) ->
	    Res = [I|L];
	    Res = L
	),
	I1 is I + 1,
	inSection(T, Stmts, I1, L).

% unsafe - istnieje przeplot do złego stanu - nie ma bezpieczeństwa
% i to jest ścieżka stanów do niego prowadząca (odwrotna)
unsafe(Program, N, State, Un) :-
	traverse(Program, N, State, [], [], _, [], Un).

safe(Program, N) :-
	initState(Program, N, In),
	unsafe(Program, N, In, []).

findCollision(Program, N, error(Err2, Numbers)) :-
	initState(Program, N, In),
	unsafe(Program, N, In, [error(Err1, Numbers)|_]),
	reverse(Err1, Err2).

traverse(Program, _, State, Stack, Vis, Vis, Un,[error(Stack, L)|Un]) :-
	collision(Program, State, L),
	!. % brzydkie - dodać niżej nie kolizja 

traverse(Program, _, State, _, Vis, Vis, Un, Un) :-
	% pewnie nieprawda że kolizja - czy dodać?
	\+ collision(Program, State),
	member(State, Vis). % ew. odcięcie

traverse(Program, N, State, Stack, Vis1, Vis, Un1, Un) :-
	\+ collision(Program, State),
	\+ member(State, Vis1),
	traverse(Program, N, State, 0, Stack, [State|Vis1], Vis, Un1, Un).



traverse(_, N, _, N, _, Vis, Vis, Un, Un).

traverse(program(V, A, P), N, State, Id, Stack, Vis1, Vis, Un1, Un) :-
	Id < N,
	step(program(V, A, P), N, State, Id, Out),
	findCurrent(State, Id, Cur),
	traverse(program(V, A, P), N, Out, [(Id, Cur)|Stack], Vis1, Vis2, Un1, Un2),
	Id1 is Id + 1,
	traverse(program(V, A, P), N, State, Id1, Stack, Vis2, Vis, Un2, Un).

% findCurrent(Stan, Proces, Instrukcja) == licznik rozkazów Procesu
% wskazuje na Instrukcja
findCurrent(state(_, _, Ps), Id, Cur) :-
	member(Id, Ps, Cur).

% Procedura główna
verify(N, File) :-
	(N =< 0 ->
	    write('Error: parametr 0 powinien byc liczba > 0'), nl, fail
	; true ),
	readProgram(File, Program),
	(safe(Program, N) ->
	    write('Program jest poprawny (bezpieczny).')
	;
	    handleCollision(Program, N)
	).

% readProgram(Plik, Program)
readProgram(File, program(Vs, As, Stmts)) :-
	catch(open(File, read, F),
	      _,
	      (format('Error: brak pliku o nazwie - ~s', [File]), nl, fail)),
	read(F, vars(Vs)),
	read(F, arrays(As)),
	read(F, program(Stmts)),
	close(F).

handleCollision(Program, N) :-
	findCollision(Program, N, error(Inter, [Num1, Num2|_])),
	length(Inter, N1),
	N2 is N1 + 1,
	format('Program jest niepoprawny: stan nr ~d nie jest bezpieczny.',
	       [N2]), nl,
	write('Niepoprawny przeplot:'), nl,
	writeInterlacing(Inter),
	format('Procesy w sekcji: ~d, ~d.', [Num1,Num2]).

% Wypisuje przeplot
writeInterlacing([]).
writeInterlacing([(Id, Pos)|T]) :-
	Pos1 is Pos + 1,
	format('  Proces ~d:   ~d',[Id, Pos1]), nl,
	writeInterlacing(T).
