-module(palindrome).
-include_lib("eunit/include/eunit.hrl").
-export([palindrome/1]).

% Instead of comparing whole string to whole
% reversed string, split string in half and
% compare left half with reversed right half.
% If there is an odd length, the right half
% will have an extra character, which is
% ignored.
-spec palindrome(string()) -> boolean().
palindrome(String) ->
    Xs = letters_only(String),
    Split = length(Xs) div 2,
    {Left, Right} = lists:split(Split, Xs),
    palindrome(Left, lists:reverse(Right)).

palindrome([], _) ->
    true;
palindrome([X|Xs], [Y|Ys]) ->
    if
        X == Y -> palindrome(Xs, Ys);
        % X is upper case Y
        X + 32 == Y -> palindrome(Xs, Ys);
        % X is lower case Y
        X - 32 == Y -> palindrome(Xs, Ys);
        true -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%% Utilities %%%%%%%%%%%%%%%%%%%%%%%%%%%%
letters_only(Xs) ->
    letters_only(Xs, []).
letters_only([], Acc) ->
    lists:reverse(Acc);
letters_only([X|Xs], Acc) when X >= $a andalso X =< $z ->
    letters_only(Xs, [X|Acc]);
letters_only([X|Xs], Acc) when X >= $A andalso X =< $Z ->
    letters_only(Xs, [X|Acc]);
letters_only([_|Xs], Acc) ->
    letters_only(Xs, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%
palindrome_test() ->
    [?assert(palindrome("Madam I\'m Adam") == true),
     ?assert(palindrome("abcdcba") == true),
     ?assert(palindrome("abccba") == true),
     ?assert(palindrome("abccbc") == false)
    ].

