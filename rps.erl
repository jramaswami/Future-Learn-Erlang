% Future Learn :: Functional Programming in Erlang :: Modelling the basics of rock-paper-scissors
% @author jramaswami

-module(rps).
-include_lib("eunit/include/eunit.hrl").
-export([beat/1, lose/1, result/2, tournament/2]).

beat(rock) ->
    paper;
beat(paper) ->
    scissors;
beat(scissors) ->
    rock.

lose(rock) ->
    scissors;
lose(scissors) ->
    paper;
lose(paper) ->
    rock.

% It is possible to use atoms, e.g. draw, win, lose, to represent
% the result of the round.  However, using 0, 1, and -1 allows you
% to use sum to easily get the required numerical result.
result(Left, Left) ->
    0;
result(Left, Right) ->
    case Left == beat(Right) of
        true -> 1;
        false -> -1 
    end.

tournament(Lefts, Rights) ->
    lists:sum(lists:zipwith(fun result/2, Lefts, Rights)).

%%%%%%%%%%%%%%%%%%%%%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%
result_test() ->
    [?assert(result(rock, rock) == 0),
     ?assert(result(paper, paper) == 0),
     ?assert(result(scissors, scissors) == 0),
     ?assert(result(rock, paper) == -1),
     ?assert(result(paper, scissors) == -1),
     ?assert(result(scissors, rock) == -1),
     ?assert(result(rock, scissors) == 1),
     ?assert(result(scissors, paper) == 1),
     ?assert(result(paper, rock) == 1)
    ].

tournament_test() ->
    [?assert(tournament([rock,rock,paper,paper],[rock,paper,scissors,rock]) == -1)
    ].
