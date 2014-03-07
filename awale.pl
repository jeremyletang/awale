   % Init some stuff

:- dynamic(state/2).
:- dynamic(score/2).
:- dynamic(current_player/1).
state(human, [4,4,4,4,4,4]).
state(ia, [4,4,4,4,4,4]).
score(human, 0).
score(ia, 0).
current_player(human).

    % States / Score utils

set_state(Player, NewState) :-
    retract(state(Player, _)),
    assert(state(Player, NewState)).

set_score(Player, NewScore) :-
    retract(score(Player, _)),
    assert(score(Player, NewScore)).

set_player:-
    current_player(Player),
    Player == human ->
    retract(current_player(_)),
    assert(current_player(ia))
    ; retract(current_player(_)),
    assert(current_plauer(human)).

    % List utils

set(Lin, Index, Value, Lout) :-
    length(Ltmp, Index),
    append(Ltmp, [_Garbage|Tail], Lin),
    append(Ltmp, [Value|Tail], Lout).

get(Lin, Index, Value) :-
    nth0(Index, Lin, Value).

    % Player play

jouer(Position) :-
    Position > 6 ->
    writeln('Impossible de selectionner une position superieur a la case 6')
    ; Position < 1 ->
    writeln('Impossible de selectionner une position inferieur a la case 1')
    ; current_player(Player),
    Player == human ->
    writeln('human play'),
    set_player,
    draw_game
    ; ansi_format([fg(red)], 'C\'est au tour de l\'IA de jouer maintenant !', []).

    % IA play

jouer_ia:-
    current_player(Player),
    Player == ia ->
    writeln('ia play'),
    set_player,
    draw_game
    ;  ansi_format([fg(red)], 'C\'est au tour du joueur de jouer maintenant !', []).

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
    state(human, StateHuman),
    state(ia, StateIA),
    score(human, ScoreHuman),
    score(ia, ScoreIA),
    write('\n'),
    write('----------------- PLATEAU DE JEU ---------------            \
---- SCORE ----\n'),
    write('------------------------------------------------            \
---------------\n'),
    write('|  IA      |  '),
    draw_values(StateIA, 0),
    write(ScoreIA),
    write('      |\n'),
    write('------------------------------------------------            \
---------------\n'),
    write('|  Humain  |  '),
    draw_values(StateHuman, 0),
    write(ScoreHuman),
    write('      |\n'),
    write('------------------------------------------------            \
---------------\n\n').

% Draw the game

%:- draw_game.
