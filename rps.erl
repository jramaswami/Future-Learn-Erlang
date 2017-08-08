% Future Learn :: Functional Programming in Erlang :: Strategies exercises
% In these exercises you’ll use functions and higher-order functions to define
% strategies and game-playing functions for the rock-paper-scissors game.
% @author jramaswami

-module(rps).
-include_lib("eunit/include/eunit.hrl").
-export([play/1, play_two/3, tournament/2, val/1, enum/1, beats/1, echo/1,
         cycle/1, rock/1, rand/1, no_repeat/1, most_frequent_strategy/1,
         least_frequent_strategy/1, make_const_strategy/1, 
         make_random_strategy/1, make_choose_best_strategy/1,
         strategy_vs_strategy/3, max_lead_strategy_vs_strategy/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Play Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Play one strategy against another, for N moves. 
% See the Strategy vs. Strategy section below for the meat of the work.
play_two(StrategyL,StrategyR,N) ->
    play_two_result_message(strategy_vs_strategy(StrategyL, StrategyR, N)).

play_two_result_message(0) -> "It is a draw.";
play_two_result_message(Outcome) when Outcome > 0 -> "Left wins!";
play_two_result_message(Outcome) when Outcome < 0 -> "Left loses!".

% Interactively play against a strategy, provided as argument.
% Provided by instructor.
play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[], 0).

% Tail recursive loop for play/1.
% Provided by instructor.
play(Strategy,Moves,Outcome) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
        stop ->
            io:format("Stopped~n~s~n", [play_result_message(Outcome)]);
        _    ->
            Result = result(Play,Strategy(Moves)),
            io:format("Result: ~p~n",[Result]),
            play(Strategy,[Play|Moves], Outcome + outcome(Result))
    end.

play_result_message(0) -> "It is a draw.";
play_result_message(Outcome) when Outcome > 0 -> "You win!";
play_result_message(Outcome) when Outcome < 0 -> "You lose!".

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Auxiliary functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Transform shorthand atoms to expanded form.
% Provided by instructor.
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% Result of one set of plays.
% Provided by instructor.
result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% Result of a tournament.
% Provided by instructor.
tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% Transform 0, 1, 2 to rock, paper, scissors and vice versa.
% Provided by instructor.
enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% Give the play which the argument beats.
% Provided by instructor.
beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Strategies %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Play what your opppenent played in the last round.  
% Provided by instructor.
echo([]) ->
     paper;
echo([Last|_]) ->
    Last.

% Play rock every time.  
% Provided by instructor.
rock(_) ->
    rock.

% Assume that your opponent never repeats herself: if you know this you can
% make a choice that will never lose.  This choice is the one that is the
% best of the remaining two choices that are not the opponent's previous move.
no_repeat([]) ->
    paper;
no_repeat([rock|_]) ->
    scissors;
no_repeat([scissors|_]) ->
    paper;
no_repeat([paper|_]) ->
    rock.

% Function that creates a function that will constantly play the given move.
% Example usage:  play_two(make_const_strategy(paper), make_const_strategy(rock), 5).
make_const_strategy(Move) ->
    fun(_) -> Move end.

% Cycles through the three choices in some order.
cycle(Xs) ->
    case length(Xs) rem 3 of
        0 -> rock;
        1 -> paper;
        2 -> scissors
    end.

% Make a random choice each time; you may want to use the random:uniform/1
% function so that random:uniform(N) returns a random choice of 1,2, … N with
% equal probability each time.
rand(_) ->
    case rand:uniform(3) of
        1 -> rock;
        2 -> paper;
        3 -> scissors
    end.

% Apply an analysis to the previous plays and choose the least frequent,
% assuming that in the long run your opponent will play each choice equally.
least_frequent_strategy(OppMoves) ->
    element(1, dict:fold(fun pick_least/3, {rock, infinity}, 
                         lists:foldl(fun increment_counter/2, dict:new(), OppMoves))).

% Apply an analysis to the previous plays and choose the most frequent,
% assuming that in the long run your opponent is going to play that choice more
% often than the others.
most_frequent_strategy(OppMoves) ->
    element(1, dict:fold(fun pick_max/3, {rock, 0}, 
                         lists:foldl(fun increment_counter/2, dict:new(), OppMoves))).

% Helper functions for least_frequent_strategy/1 and most_frequent_strategy/1.
increment_counter(Key, Dict) ->
    dict:update_counter(Key, 1, Dict).

pick_least(Key, Value, {_, infinity}) ->
    {Key, Value};
pick_least(Key, Value, {PrevMinKey, PrevMinValue}) ->
    case Value < PrevMinValue of
        true -> {Key,Value};
        false -> {PrevMinKey,PrevMinValue}
    end.

pick_max(Key, Value, {PrevMaxKey, PrevMaxValue}) ->
    case Value > PrevMaxValue of
        true -> {Key,Value};
        false -> {PrevMaxKey, PrevMaxValue}
    end.

% Define a strategy that takes a list of strategies and each play chooses a
% random one to apply.
%
% Function make_random_strategy/1 returns a function that will, for each
% round, choose and apply a random strategy.
% Example usage: 
%   play_two(make_random_strategy([fun echo/1, fun rand/1]), fun rock/1]).
make_random_strategy(Strategies=[_S|_Ss]) ->
    fun(OppMoves) -> 
        Strategy = lists:nth(rand:uniform(length(Strategies)), Strategies),
        Strategy(OppMoves)
    end.

% Define a strategy that takes a list of strategies and each play chooses from
% the list the strategy which gets the best result when played against the list
% of plays made so far.
%
% Function make_choose_best_strategy/1 returns a function that will choose,
% for each round, and apply the best strategy.
% Example usage: 
%   play_two(make_choose_best_strategy([fun echo/1, fun rand/1]), fun rock/1]).
make_choose_best_strategy([Strategy|Strategies]) ->
    fun(OppMoves) -> 
        Best = choose_best_strategy([Strategy|Strategies], OppMoves, {Strategy, 0}),
        Best(OppMoves)
    end.

choose_best_strategy([], _, {BestStrategy, _}) ->
    BestStrategy;
choose_best_strategy([Strategy|Strategies], OppMoves, {BestStrategy, BestScore}) ->
    NewScore = tournament(apply_strategy(Strategy, OppMoves), OppMoves),
    io:format("~w ~w ~n", [Strategy, NewScore]),
    case NewScore > BestScore of
        true -> choose_best_strategy(Strategies, OppMoves, {Strategy, NewScore});
        false -> choose_best_strategy(Strategies, OppMoves, {BestStrategy, BestScore})
    end.

apply_strategy(Strategy, OppMoves) ->
    apply_strategy(Strategy, OppMoves, []).
apply_strategy(_, [], _) ->
    [];
apply_strategy(Strategy, [Move|FutureMoves], PastMoves) ->
    [Strategy(PastMoves)| apply_strategy(Strategy, FutureMoves, [Move|PastMoves])].

%%%%%%%%%%%%%%%%%%%%%%%%%%% Strategy vs Strategy %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define a function that takes three arguments: two strategies and a number N,
% and which plays the strategies against each other for N turns. At each stage
% the function should output the result of the round, and it should show the
% result of the tournament at the end.
strategy_vs_strategy(LeftStrategy, RightStrategy, Rounds) ->
    strategy_vs_strategy(LeftStrategy, [], RightStrategy, [], Rounds).

strategy_vs_strategy(_, _, _, _, 0) ->
    0;
strategy_vs_strategy(LeftStrategy, LeftMoves, RightStrategy, RightMoves, Rounds) ->
    LeftMove = LeftStrategy(RightMoves),
    RightMove = RightStrategy(LeftMoves),
    Outcome = outcome(result(LeftMove, RightMove)),
    Outcome + strategy_vs_strategy(
                LeftStrategy, [LeftMove|LeftMoves],
                RightStrategy, [RightMove|RightMoves],
                Rounds - 1
               ).

% You could also choose to modify this so that the game is ended when one
% player is more than M points ahead of the other, for example.
max_lead_strategy_vs_strategy(LeftStrategy, RightStrategy, Rounds, MaxLead) ->
    max_lead_strategy_vs_strategy(LeftStrategy, [], RightStrategy, [], Rounds, MaxLead, 0).

max_lead_strategy_vs_strategy(_, _, _, _, 0, _, Acc) ->
    Acc;
max_lead_strategy_vs_strategy(_, _, _, _, _, MaxLead, Acc) when MaxLead == abs(Acc) ->
    Acc;
max_lead_strategy_vs_strategy(LeftStrategy, LeftMoves, RightStrategy, RightMoves, Rounds, MaxLead, Acc) ->
    LeftMove = LeftStrategy(RightMoves),
    RightMove = RightStrategy(LeftMoves),
    Outcome = outcome(result(LeftMove, RightMove)),
    max_lead_strategy_vs_strategy(LeftStrategy, [LeftMove|LeftMoves], 
                                  RightStrategy, [RightMove|RightMoves], 
                                  Rounds - 1,
                                  MaxLead, 
                                  Acc + Outcome
                                 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cycle_test() ->
    [?assert(cycle([]) == rock),
     ?assert(cycle([rock]) == paper),
     ?assert(cycle([rock,rock]) == scissors),
     ?assert(cycle([rock,rock,rock]) == rock),
     ?assert(cycle([rock,rock,rock,rock]) == paper),
     ?assert(cycle([rock,rock,rock,rock,rock]) == scissors)
    ].

no_repeat_test() ->
    [?assert(no_repeat([rock]) == scissors),
     ?assert(no_repeat([paper]) == rock),
     ?assert(no_repeat([scissors]) == paper),
     ?assert(result(no_repeat([rock]), scissors) =/= lose),
     ?assert(result(no_repeat([rock]), paper) =/= lose),
     ?assert(result(no_repeat([paper]), scissors) =/= lose),
     ?assert(result(no_repeat([paper]), rock) =/= lose),
     ?assert(result(no_repeat([scissors]), rock) =/= lose),
     ?assert(result(no_repeat([scissors]), paper) =/= lose)
    ].

least_frequent_strategy_test() ->
    [?assert(least_frequent_strategy([rock,rock,scissors,rock,paper,paper]) == scissors),
     ?assert(least_frequent_strategy([rock,rock,rock]) == rock)
    ].

most_frequent_strategy_test() ->
    [?assert(most_frequent_strategy([rock,rock,scissors,rock,paper,paper]) == rock),
     ?assert(most_frequent_strategy([rock,rock,rock]) == rock)
    ].

make_choose_best_strategy_test() ->
    [?assert(strategy_vs_strategy(
               make_choose_best_strategy(
                 [make_const_strategy(paper), fun rand/1, fun echo/1]
                ), 
               fun rock/1, 10) == 10
            )
    ].

max_lead_strategy_vs_strategy_test() ->
    [?assert(max_lead_strategy_vs_strategy(make_const_strategy(paper), make_const_strategy(rock), 100, 10) == 10),
     ?assert(max_lead_strategy_vs_strategy(make_const_strategy(rock), make_const_strategy(paper), 100, 10) == -10)
    ].

play_two_test() ->
    [?assert(play_two(make_const_strategy(paper), make_const_strategy(rock),10) == "Left wins!"),
     ?assert(play_two(make_const_strategy(rock), make_const_strategy(paper),10) == "Right wins!"),
     ?assert(play_two(make_const_strategy(scissors), make_const_strategy(scissors),10) == "It is a draw.")
    ].
