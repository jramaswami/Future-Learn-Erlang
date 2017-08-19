% What happens to the processes when you kill the echo process?
%
% The talk process exits with a badarg error because it tries
% to send a message to a process that is not registered, as 
% the previously registered echo process is now dead.
%
% What happens when you kill either process? (Before adding 1
% second delay between EXIT message from echo process and
% restarting it.)
% 
% Note: there appears to be an error in the assignment.  The
% delay is already in the echo process, so I actually had to
% remove it to see the different behaviors.
%
% If you kill the talk or echo process, without any delay, 
% everything just starts for example,
% 
% {<0.69.0>, 19 sent}.
% 19 echoed.
% > exit(whereis(talk), kill).
% worked re-spawned as Pid <0.71.0>.
% {<0.71.0>,0} sent.
% 0 echoed.
% true
% {<0.71.0>,1} sent.
% 1 echoed.
% 
% After adding the delay, killing the echo process causes an error, 
% but then everything respawns and starts over:
% 
% {<0.71.0>,15} sent.
% 15 echoed.
% 4> exit(whereis(echo),kill).
% true
% 5 >
% =ERROR REPORT==== 19-Aug-2017::14:41:28 ===
% Error in process <0.71.0> with exit value:
% {badarg,[{talk,work,1,[{file,"talk.erl"},{line,9}]}]}
% echo re-spawned.
% worked re-spawned as Pid <0.75.0>.
% {<0.75.0>,0} sent.
% 0 echoed.
% {<0.75.0>,1} sent.
% 1 echoed.
%
% The error is caused by the fact that the talk process 
% tries to send a message to an unregistered process
% as the echo server has not yet been restarted
% because of the delay.


-module(super).
-export([start/0, stop/0, stop/1, super/0]).

% Start the super, talk, and echo.
start() ->
    register(super, spawn(?MODULE,super,[])).

% Stop the super, talk, and echo.
stop() ->
    exit(whereis(super), kill).

% Stop the given process, e.g. super:stop(talk).
stop(Proc) ->
    exit(whereis(Proc), kill).

super() ->
    process_flag(trap_exit, true),
    E = spawn_link(echo,listener,[]),
    register(echo,E),
    io:format("echo spawned.~n"),
    T = spawn_link(talk,worker,[]),
    register(talk,T),
    io:format("worked spawned as Pid ~w.~n",[whereis(talk)]),
    loop(E,T).

loop(E,T) ->
    receive
        {'EXIT', T, _} -> 
            NewT = spawn_link(talk,worker,[]),
            register(talk,NewT),
            io:format("worked re-spawned as Pid ~w.~n",[whereis(talk)]),
            loop(E,NewT);
        {'EXIT', E, _} -> 

            NewE = spawn_link(echo,listener,[]),
            register(echo,NewE),
            io:format("echo re-spawned.~n"),
            loop(NewE,T)
    end.
