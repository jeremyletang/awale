% Init some stuff

:- dynamic(state/2).
:- dynamic(score/2).
:- dynamic(current_player/1).
state(human, [4,4,4,4,4,4]).
state(ia, [4,4,4,4,4,4]).
:- assert(state(save_human, [4,4,4,4,4,4])).
:- assert(state(save_ia, [4,4,4,4,4,4])).
score(human, 0).
score(ia, 0).
:- assert(score(save_human, 0)).
:- assert(score(save_ia, 0)).
current_player(human).


% States / Score utils

set_state(Player, NewState) :-
    retract(state(Player, _)),
    assert(state(Player, NewState)).

copy_state(Player, A) :-
    state(Player, T),
    get(T, 0, V1),
    get(T, 1, V2),
    get(T, 2, V3),
    get(T, 3, V4),
    get(T, 4, V5),
    get(T, 5, V6),
    A = [V1, V2, V3, V4, V5, V6].

update_states_from_saves:-
    copy_state(save_ia, NewIa),
    copy_state(save_human, NewHuman),
    set_state(human, NewHuman),
    set_state(ia, NewIa).

set_score(Player, NewScore) :-
    retract(score(Player, _)),
    assert(score(Player, NewScore)).

set_player:-
    current_player(Player),
    Player == human ->
    retract(current_player(_)),
    assert(current_player(ia))
    ; retract(current_player(_)),
    assert(current_player(human)).

% List utils

set(Lin, Index, Value, Lout) :-
    length(Ltmp, Index),
    append(Ltmp, [_Garbage|Tail], Lin),
    append(Ltmp, [Value|Tail], Lout).

get(Lin, Index, Value) :-
    nth0(Index, Lin, Value).

copy(List1, List2) :-
    inner_copy(List1,List2).

inner_copy([],[]).
inner_copy([Head|Next1],[Head|Next2]) :- inner_copy(Next1,Next2).

% Player play

test_slot_empty(Slot) :-
    state(human, State),
    get(State, Slot, Value),
    Value > 0 ->
    true
    ; ansi_format([fg(red)], 'Vous ne pouver jouer une case vide!', []),
    fail.

dispatch_ia(Slot, Seeds, Player) :-
    Seeds > 0 ->
    (
     Slot @>= 0 ->
     copy_state(save_ia, State),
     get(State, Slot, SlotSeeds),
     NewSlotSeeds is SlotSeeds + 1,
     set(State, Slot, NewSlotSeeds, NewState),
     set_state(save_ia, NewState),
     NewSeeds is Seeds - 1,
     NewSlot is Slot - 1,
     dispatch_ia(NewSlot, NewSeeds, Player)
     ; dispatch_player(0, Seeds, Player)
    )
    ; true.


dispatch_player(Slot, Seeds, Player) :-
    Seeds > 0 ->
    (
     Slot @=< 5 ->
     copy_state(save_human, State),
     get(State, Slot, SlotSeeds),
     NewSlotSeeds is SlotSeeds + 1,
     set(State, Slot, NewSlotSeeds, NewState),
     set_state(save_human, NewState),
     NewSeeds is Seeds - 1,
     NewSlot is Slot + 1,
     dispatch_player(NewSlot, NewSeeds, Player)
     ; dispatch_ia(5, Seeds, Player)
    )
    ; true.

apply_change_player(Slot) :-
    test_slot_empty(Slot),
    copy_state(human, A),
    get(A, Slot, Seeds),
    set(A, Slot, 0, B),
    set_state(human, B),
    NewSlot is Slot + 1,
    dispatch_player(NewSlot, Seeds, human),
    update_states_from_saves,
    set_player,
    draw_game.

jouer(Position) :-
    Position > 6 ->
    writeln('Impossible de selectionner une position superieur a la case 6')
    ; Position < 1 ->
    writeln('Impossible de selectionner une position inferieur a la case 1')
    ; current_player(Player),
    Player == human ->
    Slot is Position - 1,
    apply_change_player(Slot)
    ; ansi_format([fg(red)], 'C\'est au tour de l\'IA de jouer maintenant !',
          []).

% IA play

jouer_ia:-
    current_player(Player),
    Player == ia ->
    writeln('ia play'),
    set_player,
    draw_game
    ;  ansi_format([fg(red)], 'C\'est au tour du joueur de jouer maintenant !',
           []).

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

:- draw_game.
