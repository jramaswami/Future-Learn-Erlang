-module(palindrome_server).
-export([server/1, server/0, forwarder/2, replicator/0]).

% A palindrome checking server:
% Define a function server/1 that accepts messages of the form
%               {check,"Madam I\'m Adam"}
% and returns results like
%               {result,"\"Madam I\'m Adam\" is a palindrome"}
% If it is sent any other format of message, such as
%               stop
% the server should stop, by terminating its operation. The 
% argument to server/1 should be the Pid of the process to which 
% results are to be returned.
%
% Makes use of previous assignment's function to check if a string
% is a palindrome in palindrome.erl.
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

% Modifying your server:
% Can you modify the definition of your server to take requests from multiple
% clients? For this to work, each client will need to pass its Pid as part of
% each message, and the server will have to extract that information as the
% destination of its reply.
server() ->
    receive
        {check, P, Caller} ->
            io:format("Palindrome Server ~w: checking ~s for ~w~n", [self(), P, Caller]),
            case palindrome:palindrome(P) of
                false -> Caller ! {result, "\"" ++ P ++ "\" is not a palindrome."};
                true -> Caller ! {result, "\"" ++ P ++ "\" is a palindrome."}
            end,
            server();
        stop ->
            io:format("Palindrome Server ~w: stopping~n", [self()]),
            ok
    end.

forwarder(Parent, PalindromeServer) ->
    receive
        {forward, P} ->
            io:format("Forwarder ~w: forwarding palindrome \"~s\" to palindrome server~n", [self(), P]),
            PalindromeServer ! {check, P, self()},
            forwarder(Parent, PalindromeServer);
        {result, S} ->
            io:format("Forwarder ~w: forwarding result \"~s\" to parent ~w~n", [self(), S, Parent]),
            Parent ! {forwarded_result, S},
            forwarder(Parent, PalindromeServer);
        stop ->
            io:format("Forwarder ~w: stopping~n", [self()]),
            ok
    end.

% Replicating the server:
% Suppose that you need to replicate the server because of the volume of
% traffic.  One way of doing this is to put in place a front end which
% distributes work to the two servers, which then return their results 
% directly to the clients.
%
% Note this implementation rotates the servers.
replicator() ->
    receive
        {check, P, Caller} ->
            Server1 = spawn(?MODULE, server, []),
            io:format("Spawning palindrome checker ~w~n", [Server1]),
            Server1 ! {check, P, Caller},
            replicator([Server1]);
        stop ->
            ok
    end.
replicator([Server1]) ->
    receive
        {check, P, Caller} ->
            Server2 = spawn(?MODULE, server, []),
            io:format("Spawning palindrome checker ~w~n", [Server2]),
            Server2 ! {check, P, Caller},
            replicator([Server1, Server2]);
        stop ->
            ok
    end;
replicator([Server1, Server2]) ->
    receive
        {check, P, Caller} ->
            Server1 ! {check, P, Caller},
            replicator([Server2, Server1]);
        stop ->
            ok
    end.
