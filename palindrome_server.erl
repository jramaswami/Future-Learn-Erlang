-module(palindrome_server).
-export([server/1]).

server(Caller) ->
    receive
        {check, P} ->
            case palindrome:palindrome(P) of
                false -> Caller ! {result, "\"" ++ P ++ "\" is not a palindrome."};
                true -> Caller ! {result, "\"" ++ P ++ "\" is a palindrome."}
            end,
            server(Caller);
        stop ->
            ok
    end.
