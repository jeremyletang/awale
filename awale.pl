   % Init some stuff

:- dynamic(state/2).
:- dynamic(score/2).
state(human, [4,4,4,4,4,4]).
state(ia, [4,4,4,4,4,4]).
score(human, 0).
score(ia, 0).

    % States / Score utils

set_state(Player, NewState) :-
    retract(state(Player, _)),
    assert(state(Player, NewState)).

set_score(Player, NewScore) :-
    retract(score(Player, _)),
    assert(score(Player, NewScore)).

    % List utils

set(Lin, Index, Value, Lout) :-
    length(Ltmp, Index),
    append(Ltmp, [_Garbage|Tail], Lin),
    append(Ltmp, [Value|Tail], Lout).

get(Lin, Index, Value) :-
    nth0(Index, Lin, Value).

    % Drawing

draw_values(List, X) :-
X < 6 ->
    get(List, X, Value),
    write(Value),
    write('  |  '),
    N is X + 1,
    draw_values(List, N)
    ; write('          |      ').

draw_game:-
    write('\n'),
    write('----------------- PLATEAU DE JEU ---------------            ---- SCORE ----\n'),
    write('------------------------------------------------            ---------------\n'),
    write('|  IA      |  '),
    state(ia, StateIA),
    draw_values(StateIA, 0),
    score(ia, ScoreIA),
    write(ScoreIA),
    write('     |\n'),
    write('------------------------------------------------            ---------------\n'),
    write('|  Humain  |  '),
    state(human, StateHuman),
    draw_values(StateHuman, 0),
    score(human, ScoreHuman),
    write(ScoreHuman),
    write('     |\n'),
    write('------------------------------------------------            ---------------\n\n').
