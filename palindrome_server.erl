-module(palindrome_server).
-export([server/1, server/0, forwarder/2]).

% A palindrome checking server
% Define a function server/1 that accepts messages of the form
%               {check,"Madam I\'m Adam"}
% and returns results like
%               {result,"\"Madam I\'m Adam\" is a palindrome"}
% If it is sent any other format of message, such as
%               stop
% the server should stop, by terminating its operation. The 
% argument to server/1 should be the Pid of the process to which 
% results are to be returned.
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

% Modifying your server
% Can you modify the definition of your server to take requests from multiple
% clients? For this to work, each client will need to pass its Pid as part of
% each message, and the server will have to extract that information as the
% destination of its reply.
server() ->
    receive
        {check, P, Caller} ->
            case palindrome:palindrome(P) of
                false -> Caller ! {result, "\"" ++ P ++ "\" is not a palindrome."};
                true -> Caller ! {result, "\"" ++ P ++ "\" is a palindrome."}
            end,
            server();
        stop ->
            ok
    end.

forwarder(Parent, PalindromeServer) ->
    receive
        {forward, P} ->
            PalindromeServer ! {check, P, self()},
            forwarder(Parent, PalindromeServer);
        {result, S} ->
            Parent ! {forwarded_result, S},
            forwarder(Parent, PalindromeServer);
        stop ->
            ok
    end.

