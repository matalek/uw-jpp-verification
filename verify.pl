% Aleksander Matusiak
% JPP 2016 - zadanie zaliczeniowe 3. (Prolog)

:- ensure_loaded(library(lists)).
:- op(700, xfx, <>). % konieczna definicja operatora różności

% Reprezentacja programu:

% state(ZmienneProste, Tablice, LicznikiRozkazu) -
% reprezentacja stanu systemu
% ZmienneProste = [val(Zmienna, Wartość)],
% Tablice = [val(NazwaTablicy, [Wartości])],
% LicznikiRozkazu = [LicznikRozkazu] (lista liczb całkowitych,
% numeracja instrukcji od 0 (inaczej niż w opisie))

% singleState(ZmienneProste, Tablice, LicznikRozkazu) - odpowiada
% stanowi pojedynczego procesu - zawiera globalne zmienne i tablice,
% ale tylko licznik rozkazu pojedynczego procesu

% program(Zmienne, Tablice, Cialo)

% initState(Program, LiczbaProcesów, StanPoczątkowy).
initState(program(VarsNames, ArraysNames, _), N, state(Vars, Arrays, Ips)) :-
	initVars(VarsNames, Vars),
	initArrays(ArraysNames, N, Arrays),
	initArray(N, Ips).

% initVars(ListaNazwZmiennych, ZmienneZainicjowaneZerem).
initVars([], []).
initVars([V|T1], [val(V, 0)|T2]) :-
		initVars(T1, T2).

% initArrays(ListaNazwTablic, RozmiarTablicy, ListaTablic).
initArrays([], _, []).
initArrays([V|T1], N, [val(V, Arr)|T2]) :-
	initArray(N, Arr),
	initArrays(T1, N, T2).

% initArray(Rozmiar, Lista) == Lista ma długość Rozmiar i jest
% wypełniona zerami.
initArray(0, []).
initArray(N, [0|T]) :-
	N > 0,
	N1 is N - 1,
	initArray(N1, T).

% processList(LiczbaProcesów, ListaKolejnychProcesów).
processList(0, []).
processList(N, [N1|T]) :-
	N > 0,
	N1 is N - 1,
	processList(N1, T).

% member(Indeks, Tablica, Wartość) == Wartość jest na indeksie Indeks w Tablica
% (indeksy od 0)
member(0, [H|_], H) :- !.
member(N, [_|T], Res) :-
	N > 0,
	N1 is N - 1,
	member(N1, T, Res).

% step(Program, LiczbaProcesów, StanWe, PrId, StanWy).
% Zmiana w stosunku do specyfikacji - dodanie liczby procesów!
% Liczba procesów potrzebna jest do generowania możliwych PrId.
step(program(_, _, S), N, In, Id, Out) :-
	processList(N, PL),
	member(Id, PL), % id procesu musi być w zakresie
	stepAux(S, In, Id, Out).

% stepAux(+Instrukcje, +StanWe, +PrId, StanWy).
stepAux(S, state(V1, A1, P1), Id, state(V2, A2, P2)) :-
	member(Id, P1, Cur1), % która instrukcja dla danego procesu
	member(Cur1, S, Stmt),
	stepSingle(Stmt, Id,
		   singleState(V1, A1, Cur1),
		   singleState(V2, A2, Cur2)),
	setCell(Id, P1, Cur2, P2).

% setCell(Indeks, StaraLista, Wartość, NowaTablica) ==
% w NowaTablicy na indeksie Indeks stoi Wartość, reszta
% wartości jest niezmieniona (numerujemy od 0)
setCell(0, [_|T], New, [New|T]) :- !.
setCell(Id1, [H|T1], New, [H|T2]) :-
	Id1 > 0,
	Id2 is Id1 - 1,
	setCell(Id2, T1, New, T2).

% setVariable(Lista, Zmienna, Wartość, NowaLista) ==
% W NowaLista zmiennej Zmienna przypisana jest wartość
% Wartość, reszta zmiennych i ich wartości się nie różni
setVariable([val(X, _)|T], X, N, [val(X, N)|T]) :- !.
setVariable([val(Y, V)|T1], X, N, [val(Y, V)|T2]) :-
	X \= Y,
	setVariable(T1, X, N, T2).

% setArrayCell(StaraListaTablic, NazwaTablicy, Indeks,
% Wartość, NowaListaTablic) == NowaListaTablic różni
% się od StaraListaTablic tym, że w tablicy NazwaTablicy
% na indeksie Indeks jest wartość Wartość
setArrayCell([val(X, Arr)|T], X, I, N, [val(X, NewArray)|T]) :-
	!,
	setCell(I, Arr, N, NewArray). 
setArrayCell([val(Y, V)|T1], X, I, N, [val(Y, V)|T2]) :-
	X \= Y,
	setArrayCell(T1, X, I, N, T2).

% stepSingle(Insrukcja, Proces, StanPojedynczyPoczątkowy,
% StanPojednczyKońcowy) == StanPojedynczyKońcowy odpowiada wykonaniu
% Instrukcji przez Proces w StanPojedynczyPoczątkowy
stepSingle(assign(X, Exp), Id,
	   singleState(V1, A1, P1), singleState(V2, A1, P2)) :-
	atom(X),
	!,
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
eval(V, Vs, _, _, N) :-
	atom(V),
	V \= pid,
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
% collision(Program, Stan) == w stanie co najmniej 2 procesy są
% w sekcji krytycznej, L to lista tych procesów)
collision(program(_, _, Stmts), state(_, _, Ps), L) :-
	inSection(Ps, Stmts, L),
	length(L, In),
	In > 1.

% collision(Program, Stan) == w stanie co najmniej 2 procesy są
% w sekcji krytycznej
collision(Program, State) :- collision(Program, State, _).

% inSection(ListaLiczników, TreśćProgramu, ListaProcesówWSekcji).
inSection(Ps, Stmts, In) :- inSection(Ps, Stmts, 0, In).

% inSection(ListaLiczników, TreśćProgramu, Proces, ListaProcWSekcji).
inSection([], _, _, []).
inSection([H|T], Stmts, I, Res) :-
	(member(H, Stmts, sekcja) ->
	    Res = [I|L];
	    Res = L
	),
	I1 is I + 1,
	inSection(T, Stmts, I1, L).


% verify(+Program, +N, -Un) == jeśli wynikowe Un jest zmienną, to Program jest
% bezpieczny dla N procesów. Wpp. znaleziono kolizję i
% Un =  error(Przeplot, NumeryProcesówSekcji, NumerNiebezpiecznegoStanu).
verify(Program, N, Un) :-
	initState(Program, N, In),
	check(Program, N, In, Un).

% check(+Program, +LiczbaProcesów, +StanPoczątkowy, -Wynik),
% gdzie Wynik będzie zmienną, jeśli nie udało się znaleźć kolizji,
% będzie zaś termem postaci error(_, _, _), jeśli ta kolizja istnieje
% (i ten term będzie odpowiadał kolizji).
check(Program, N, In, Un) :-
	traverse(Program, N, In, [], [], _, Un).

% traverse(+Program, +LiczbaProcesów, Stan, +Stos
% +OdwiedzoneStany, -NoweOdwiedzoneStany, -Wynik ) -
% NoweOdwiedzoneStany odpowiadają rekurencyjnemu
% odwiedzeniu dostępnych stanów z aktualnego stanu. Stos zawiera
% pary (IdProcesu, NrInstrukcji).
% Jeśli nie została znaleziona kolizja, to Wynik jest zmienną,
% w przeciwnym przypadku zawiera on informacje o kolizji
% (error(_, _, _)).

traverse(_, _, State, _, Vis, Vis, _) :-
	member(State, Vis), % stan było już odwiedzony
	!.

traverse(Program, _, State, Stack, Vis, Vis,
	 error(Stack, L, StateNumber)) :-
	\+ member(State, Vis),
	collision(Program, State, L),
	!,
	length(Vis, VisLength),
	StateNumber is VisLength + 1.

traverse(Program, N, State, Stack, Vis1, Vis2, Un) :-
	\+ member(State, Vis1),
	\+ collision(Program, State),
	% przeglądamy wszystkie możliwe ruchy - DFS
	traverse(Program, N, State, 0, Stack, [State|Vis1], Vis2, Un).

%traverse(Program, LiczbaProcesów, Stan, Proces, Stos, Odwiedzone,
% NoweOdwiedzone, Kolizje, NoweKolizje) == jw. tylko w danym ruchu
% mogą zostać wykonać instrukcję procesy o identyfikatorze
% >= Proces.
traverse(_, N, _, N, _, Vis, Vis, _) :- !.

traverse(Program, N, State, Id, Stack, Vis1, Vis, Un) :-
	Id < N,
	step(Program, N, State, Id, Out),
	findCurrent(State, Id, Cur),
	traverse(Program, N, Out, [(Id, Cur)|Stack], Vis1, Vis2, Un),
	Id1 is Id + 1,
	(var(Un) ->
	    % przeglądamy dalsze krawędzie
	    traverse(Program, N, State, Id1, Stack, Vis2, Vis, Un)
	; % kolizja została już znaleziona
	    true
	).

% findCurrent(Stan, Proces, Instrukcja) == licznik rozkazów Procesu
% wskazuje na Instrukcja
findCurrent(state(_, _, Ps), Id, Cur) :-
	member(Id, Ps, Cur).

% Procedura główna
verify(N, File) :-
	(\+ integer(N) ->
	    write('Error: parametr 0 powinien byc liczba'), nl, fail
	; true ),
	(N =< 0 ->
	    write('Error: parametr 0 powinien byc liczba > 0'), nl, fail
	; true ),
	readProgram(File, Program),
	verify(Program, N, Res),
	(var(Res) ->
	    write('Program jest poprawny (bezpieczny).')
	;
	    handleCollision(Res)
	).

% readProgram(Plik, Program)
readProgram(File, program(Vs, As, Stmts)) :-
	set_prolog_flag(fileerrors, off),
	see(File),
	!, % czerwone odcięcie
	read(vars(Vs)),
	read(arrays(As)),
	read(program(Stmts)),
	seen.

readProgram(File, _) :-
	format('Error: brak pliku o nazwie - ~s', [File]),nl,
	fail. % wywołanie kończy się błędem

% Wypisuje pełną informację, gdy program nie jest bezpieczny
handleCollision(error(Inter, [Num1, Num2|_], StateNumber)) :-
	format('Program jest niepoprawny: stan nr ~d nie jest bezpieczny.',
	       [StateNumber]), nl,
	write('Niepoprawny przeplot:'), nl,
	writeInterlacing(Inter),
	format('Procesy w sekcji: ~d, ~d.', [Num1,Num2]).

% Wypisuje przeplot
writeInterlacing([]).
writeInterlacing([(Id, Pos)|T]) :-
	Pos1 is Pos + 1,
	format('  Proces ~d:   ~d',[Id, Pos1]), nl,
	writeInterlacing(T).
