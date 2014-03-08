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

update_game_datas(NewHumanState, NewIaState, HumanScore, IaScore) :-
    set_state(human, NewHumanState),
    set_state(ia, NewIaState),
    score(human, TmpHumanScore),
    NewHumanScore is TmpHumanScore + HumanScore,
    score(ia, TmpIaScore),
    NewIaScore is TmpIaScore + IaScore,
    set_score(ia, NewIaScore),
    set_score(human, NewHumanScore).


set_score(Player, NewScore) :-
    retract(score(Player, _)),
    assert(score(Player, NewScore)).

set_player :-
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

% Game state

reset_game :-
    set_state(human, [4,4,4,4,4,4]),
    set_state(ia, [4,4,4,4,4,4]),
    set_score(human, 0),
    set_score(ia, 0),
    retract(current_player(_)),
    assert(current_player(human)).

win_by_seeds_quantity(Player, 0) :-
    write(Player),
    ansi_format([fg(green)], ' a gagne car il ne lui reste plus aucune graine \
!\n', []),
    reset_game.

win_by_seeds_quantity(_, _) :- true.

win_by_score(Player, Score) :-
    Score @>= 25 ->
    write(Player),
    ansi_format([fg(green)], ' a gagne car il possede un score de 25 et plus \
!\n', []),
    reset_game
    ; true.

finalize_win_by_low_seeds(Player) :-
    write(Player),
    ansi_format([fg(green)], ' a gagne car il reste moins de 6 graines en \
jeux, et c\'est lui qui possede le plus au score.', []),
    reset_game.

win_by_low_seeds :-
    score(humam, HScore),
    score(ia, IScore),
    TotalScore is IScore + HScore,
    TotalScore @>= 42 ->
    (
     HScore @> IScore ->
     finalize_win_by_low_seeds(human)
     ; IScore @> HScore ->
     finalize_win_by_low_seeds(ia)
     ; true
    )
    ; true.

check_end_game(Player) :-
    state(Player, State),
    sum_list(State, Res),
    win_by_seeds_quantity(Player, Res),
    win_by_low_seeds.

% Player play

test_slot_empty(Slot) :-
    state(human, State),
    get(State, Slot, Value),
    Value > 0 ->
    true
    ; ansi_format([fg(red)], 'Vous ne pouvez jouer une case vide!', []),
    fail.

win_slot(State, NewState, Slot, 2, AddScore) :-
    set(State, Slot, 0, TmpNewState),
    NewState = TmpNewState,
    AddScore = 2.

win_slot(State, NewState, Slot, 3, AddScore) :-
    set(State, Slot, 0, TmpNewState),
    NewState = TmpNewState,
    AddScore = 3.

win_slot(State, NewState, _, _, AddScore) :-
    NewState = State,
    AddScore = 0.

check_previous_slot(State, NewState, Slot, AddScore) :-
    get(State, Slot, SlotSeeds),
    win_slot(State, NewState, Slot, SlotSeeds, AddScore).

dispatch_ia(HumanState,
            IaState,
            NewHumanState,
            NewIaState,
            Slot,
            Seeds,
            Player,
            AddScore) :-
    Seeds > 0 ->
    (
        Slot @>= 0 ->
        get(IaState, Slot, SlotSeeds),
        NewSlotSeeds is SlotSeeds + 1,
        set(IaState, Slot, NewSlotSeeds, NewState),
        NewSeeds is Seeds - 1,
        NewSlot is Slot - 1,
        dispatch_ia(HumanState,
                    NewState,
                    NewHumanState,
                    NewIaState,
                    NewSlot,
                    NewSeeds,
                    Player,
                    AddScore)
        ; dispatch_human(HumanState,
                         IaState,
                         NewHumanState,
                         NewIaState,
                         0,
                         Seeds,
                         Player,
                         AddScore)
    )
    ; NewHumanState = HumanState,
    current_player(Player),
    Player == human ->
    PrevSlot is Slot + 1,
    check_previous_slot(IaState, TmpNewIaState, PrevSlot, AddScore),
    NewIaState = TmpNewIaState
    ; NewIaState = IaState.

dispatch_human(HumanState,
               IaState,
               NewHumanState,
               NewIaState,
               Slot,
               Seeds,
               Player,
               AddScore) :-
    Seeds > 0 ->
    (
        Slot @=< 5 ->
        get(HumanState, Slot, SlotSeeds),
        NewSlotSeeds is SlotSeeds + 1,
        set(HumanState, Slot, NewSlotSeeds, NewState),
        NewSeeds is Seeds - 1,
        NewSlot is Slot + 1,
        dispatch_human(NewState,
                       IaState,
                       NewHumanState,
                       NewIaState,
                       NewSlot,
                       NewSeeds,
                       Player,
                       AddScore)
        ; dispatch_ia(HumanState,
                      IaState,
                      NewHumanState,
                      NewIaState,
                      5,
                      Seeds,
                      Player,
                      AddScore)
    )
    ; NewIaState = IaState,
    NewHumanState = HumanState.

apply_change_human(Slot) :-
    test_slot_empty(Slot),
    copy_state(human, A),
    get(A, Slot, Seeds),
    set(A, Slot, 0, B),
    set_state(human, B),
    NewSlot is Slot + 1,
    copy_state(human, HumanState),
    copy_state(ia, IaState),
    dispatch_human(HumanState,
                   IaState,
                   NewHumanState,
                   NewIaState,
                   NewSlot,
                   Seeds,
                   human,
                   AddScore),
    update_game_datas(NewHumanState, NewIaState, AddScore, 0),
    set_player,
    check_end_game(human),
    draw_game.

jouer(Position) :-
    Position > 6 ->
    writeln('Impossible de selectionner une position superieur a la case 6')
    ; Position < 1 ->
    writeln('Impossible de selectionner une position inferieur a la case 1')
    ; current_player(Player),
    Player == human ->
    Slot is Position - 1,
    apply_change_human(Slot)
    ; ansi_format([fg(red)], 'C\'est au tour de l\'IA de jouer maintenant !',
                  []).

% IA play

find_slot_ia(Slot) :-
    write('ok').

jouer_ia:-
    current_player(Player),
    Player == ia ->
    find_slot_ia(0),
    set_player,
    draw_game
    ; ansi_format([fg(red)], 'C\'est au tour du joueur de jouer maintenant !',
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

draw_game :-
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
    (ScoreIA < 10 ->
     write('      |\n')
     ; write('     |\n')),
    write('------------------------------------------------            \
---------------\n'),
    write('|  Humain  |  '),
    draw_values(StateHuman, 0),
    write(ScoreHuman),
    (ScoreHuman < 10 ->
     write('      |\n')
     ; write('     |\n')),
    write('------------------------------------------------            \
---------------\n\n').

notice :-
    write('Commandes: \n\tPour choisire une case utilisez la procedure '),
    ansi_format([fg(cyan)], 'jouer(X)', []),
    write(' ou X est la case que vous avez choisi.\n\tPour faire jouer l\'IA\
utilisez la procedure '),
    ansi_format([fg(cyan)], 'jouer_ia', []),
    write('.\n\tPour afficher cette notice utilisez la procedure '),
    ansi_format([fg(cyan)], 'notice', []),
    writeln('.').

% Draw the game

:- ansi_format([fg(green)], '\nBienvenue dans le jeu de l\'awale !\n\n', []).
:- notice.
:- draw_game.
