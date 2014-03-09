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

% Met a jour l'etat du plateau d'un des deux joueurs
set_state(Player, NewState) :-
    retract(state(Player, _)),
    assert(state(Player, NewState)).

% Realise une copie du plateau d'un des deux joueurs.
copy_state(Player, A) :-
    state(Player, T),
    get(T, 0, V1),
    get(T, 1, V2),
    get(T, 2, V3),
    get(T, 3, V4),
    get(T, 4, V5),
    get(T, 5, V6),
    A = [V1, V2, V3, V4, V5, V6].

% Mise a jour de tous le plateau
update_game_datas(NewHumanState, NewIaState, HumanScore, IaScore) :-
    set_state(human, NewHumanState),
    set_state(ia, NewIaState),
    score(human, TmpHumanScore),
    NewHumanScore is TmpHumanScore + HumanScore,
    score(ia, TmpIaScore),
    NewIaScore is TmpIaScore + IaScore,
    set_score(ia, NewIaScore),
    set_score(human, NewHumanScore).

% Met a jour le score d'un des joueurs.
set_score(Player, NewScore) :-
    retract(score(Player, _)),
    assert(score(Player, NewScore)).

% Specifier le joueur courant
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

% Reinitialise le jeu apres que la partie soit gagnee.
reset_game :-
    set_state(human, [4,4,4,4,4,4]),
    set_state(ia, [4,4,4,4,4,4]),
    set_score(human, 0),
    set_score(ia, 0),
    retract(current_player(_)),
    assert(current_player(human)).

% Verifie si le joueur a gagner la partie en ayant plus aucune graine de son cote.
win_by_seeds_quantity(Player, 0) :-
    write(Player),
    ansi_format([fg(green)], ' a gagne car il ne lui reste plus aucune graine \
!\n', []),
    reset_game.

win_by_seeds_quantity(_, _) :- true.

% Verifie si le joueur a gagne la partie en ayant un score totale de plus de
% 25 graines
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

% Verifie si le nombre de graine encore disponibles sur le plateau est
% inferieur a 6, dans ce cas la partie ce termine. On verifie alors
% quel joueur a le plus de graine afin de determiner le vainqueur.
win_by_low_seeds :-
    score(humam, HScore),
    score(ia, IScore),
    TotalScore is IScore + HScore,
    TotalScore @>= 42 ->
    (
     HScore @> IScore ->
     finalize_win_by_low_seeds(human)
    )
    ; true.

% Verifie si la partie est terminee.
check_end_game(Player) :-
    state(Player, State),
    sum_list(State, Res),
    win_by_seeds_quantity(Player, Res),
    score(Player, Score),
    win_by_score(Player, Score),
    win_by_low_seeds.

% Player play

% Verifie en debut de partie si la case choisit par l'utilisateur est
% vide, si oui alors un message d'erreur est emis et le joueur doit
% choisir une nouvelle case. Si non le dispatch des graines est lance.
test_slot_empty(Slot) :-
    state(human, State),
    get(State, Slot, Value),
    Value > 0 ->
    true
    ; ansi_format([fg(red)], 'Vous ne pouvez jouer une case vide!', []),
    fail.

% La case final apres le dispatch des graine du joueur possede 2 graine,
% le joueur gagne donc ces 2 graines.
win_slot(State, NewState, Slot, 2, AddScore) :-
    set(State, Slot, 0, TmpNewState),
    NewState = TmpNewState,
    AddScore = 2.

% La case final apres le dispatch des graine du joueur possede 3 graine,
% le joueur gagne donc ces 3 graines.
win_slot(State, NewState, Slot, 3, AddScore) :-
    set(State, Slot, 0, TmpNewState),
    NewState = TmpNewState,
    AddScore = 3.

% Tous les autres cas de fin de dispatch des graine, rien a gagner.
win_slot(State, NewState, _, _, AddScore) :-
    NewState = State,
    AddScore = 0.

% Verification du Slot precedant afin de avoir si le joueur gagne
% des graines ou non.
check_previous_slot(State, NewState, Slot, AddScore) :-
    get(State, Slot, SlotSeeds),
    win_slot(State, NewState, Slot, SlotSeeds, AddScore).

% Insertion des graines dans les cases du cotes ia.
% Tant que le nombre de graines est superieur a 0 ajoute une graine
% dans la case courant puis se rappel, ou appel la procedure
% dispatch_ia si on est arrive au bout des case du joueur. Si le
% nombre de graine est egale a 0 on verifie alors si le joueur courant
% est l'humain, et si la case courante possede une ou deux graine. Puis les
% donnes du plateau dans son etat final ainsi que le score sont unifie.
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
    ; NewIaState = IaState,
    NewHumanState = HumanState,
    AddScore = 0.

% Insertion des graines dans les cases du cotes humain.
% Tant que le nombre de graines est superieur a 0 ajoute une graine
% dans la case courant puis se rappel, ou appel la procedure
% dispatch_ia si on est arrive au bout des case du joueur. Si le
% nombre de graine est egale a 0 on verifie alors si le joueur courant
% est l'IA, et si la case courante possede une ou deux graine. Puis les
% donnes du plateau dans son etat final ainsi que le score sont unifie.
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
    current_player(Player),
    Player == ia ->
    PrevSlot is Slot - 1,
    check_previous_slot(HumanState, TmpNewHumanState, PrevSlot, AddScore),
    NewHumanState = TmpNewHumanState
    ; NewIaState = IaState,
    NewHumanState = HumanState,
    AddScore = 0.

% Lance le dispatch des graines dans les differentes cases, puis mets
% a jours les differentes donnees de jeux et verifie si la partie est
% terminee
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

% Procedure de jeu du joueur humain.
% Position -> La case que le joueur selectionne.
% Verifie si la case selectionne est inferieur a 1 ou superieur a 6 ( cases
% en dehors du plateau), si oui affiche un message d'erreur, si non verifie
% alors si le tour de jeu est celui du joueur humain, si oui alors le
% dispatch des graines dans les differentes cases est lance. Sinon un
% message d'erreur est affiche.
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

% Verifie si le nouvel etats du plateau, ainsi que le nouveau score sont
% meilleurs que l'ancien. Si oui le nouveau score courant est unifier avec
% le nouveau score.
check_better_states(HumanState,
                    IaState,
                    AddScore,
                    CurrentScore,
                    NewCurrentScore) :-
    AddScore > CurrentScore ->
    NewCurrentScore = AddScore,
    set_state(save_human, HumanState),
    set_state(save_ia, IaState)
    ; NewCurrentScore = CurrentScore.

% Procedure principal de l'IA
%
% Permet de passer sur les differentes case du plateau appartenant a l'IA,
% verifie d'abord si la case est vide, si oui, la procedure est rappelee
% avec en parametre la case suivante et le score courant. Sinon le dispatch
% des graines est effectues dans les differentes case puis l'etat des cases
% et le nouveau score ainsi obtenu est unifie afin de determine si le
% nouveau score est meilleur que l'ancien score (appel de check_better_states)
% enfin la procedure se rappel afin de verifier les autres cases.
%
% Lorsque toutes les cases sont verifiees le nouvel etat du plateau est
% enregistre ainsi que le nouveau score, puis on verifie si la partie est
% terminee.
find_slot_ia(Slot, CurrentScore) :-
    Slot < 6 ->
    copy_state(ia, A),
    get(A, Slot, Seeds),
    (
        Seeds == 0 ->
        NewSlot is Slot + 1,
        find_slot_ia(NewSlot, CurrentScore)
        ; copy_state(human, HumanState),
        copy_state(ia, I),
        set(I, Slot, 0, IaState),
        SlotCopy is Slot - 1,
        dispatch_ia(HumanState,
                    IaState,
                    NewHumanState,
                    NewIaState,
                    SlotCopy,
                    Seeds,
                    ia,
                    AddScore),
        check_better_states(NewHumanState,
                            NewIaState,
                            AddScore,
                            CurrentScore,
                            NewCurrentScore),
        NewSlot is Slot + 1,
        find_slot_ia(NewSlot, NewCurrentScore)
    )
    ; state(save_human, HumanState),
    state(save_ia, IaState),
    update_game_datas(HumanState, IaState,0, CurrentScore).

% Lancement du tour de jeu de l'IA
jouer_ia:-
    current_player(Player),
    Player == ia ->
    find_slot_ia(0, -1),
    set_player,
    check_end_game(ia),
    draw_game
    ; ansi_format([fg(red)], 'C\'est au tour du joueur de jouer maintenant !',
           []).

% Drawing

draw_values(List, X) :-
    X < 6 ->
    get(List, X, Value),
    write(Value),
    (Value < 10 ->
    write('  |  ')
    ; write(' |  ')),
    N is X + 1,
    draw_values(List, N)
    ; write('          |      ').

% Dessiner le plateau de jeu et les scores
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

% Afficher la documentation du jeu
notice :-
    write('Commandes: \n\tPour choisir une case utilisez la procedure '),
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
