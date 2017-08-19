-module(scenario).
-export([setup/0, client/2, client/4, random_elem/1]).

% Use this module to exercise the behaviour of the 
% hardened frequency server.

% Calling setup will launch the server and two clients: alice and bob.

setup() ->
    frequency_hardened:start(),
    register(alice, spawn(?MODULE,client,[alice,[]])),
    register(bob, spawn(?MODULE,client,[bob,[]])).

% A client, parametrised by its name (optional, but useful instrumentation),
% and the list of frequencies currently allocated to that process. Needed
% to produce calls to deallocate/1 that don't fail.
% Also
%   - parameterised on the ratio of allocates to deallocates
%   - parameterised on time that client sleeps between requests
%   - deal with case when no frequencies available: here a client fails
%   - add stop commands.
client(Id, Freqs) ->
    client(Id, Freqs, random, random).
client(Id, Freqs, SleepTime, AllocRatio) ->
    process_flag(trap_exit, true),
    receive
        {'EXIT', _Pid, _Reason} ->
            server_exited(Id, Freqs, SleepTime, AllocRatio);
        stop -> 
            io:format("~w stopping.~n", [Id]),
            ok
    after 0  ->
        io:format("~w has frequencies ~w.~n", [Id, Freqs]),
        NewFreqs = make_request(Id, Freqs, next_request(AllocRatio)),
        timer:sleep(sleep_for(SleepTime)),
        client(Id, NewFreqs, SleepTime, AllocRatio)
    end.

% Handle server crash.
server_exited(Id, Freqs, SleepTime, AllocRatio) ->
    case whereis(frequency) of
        undefined -> 
            io:format("~w is restarting frequency server.~n", [Id]),
            frequency_hardened:start();
        _ -> ok
    end,
    reallocate_frequencies(Id, Freqs, SleepTime, AllocRatio, []).

% Reallocate number of frequencies necessary.
reallocate_frequencies(Id, [], SleepTime, AllocRatio, NewFreqs) ->
    client(Id, NewFreqs, SleepTime, AllocRatio);
reallocate_frequencies(Id, [_|Freqs], SleepTime, AllocRatio, NewFreqs) ->
    reallocate_frequencies(Id, Freqs, SleepTime, AllocRatio, make_request(Id, NewFreqs, 1)).

% Get next request code.
next_request(random) ->
    rand:uniform(2);
next_request(N) when N >= 0; N =< 1 ->
    case rand:uniform() < N of
        true -> 1;
        false -> 2
    end;
next_request(_) ->
    next_request(0.5).

% Get the time that client will sleep.
sleep_for(random) ->
    500 + rand:uniform(4500);
sleep_for(N) when N >= 0; N =< 5000 ->
    N;
sleep_for(_) ->
    1000.
    
% Make allocation (1) or deallocation (2) request.  Returns 
% new list of frequencies held by this client.
make_request(Id, Freqs, 2) ->
    Len = length(Freqs),
    case Len of 
        0 -> 
            io:format("No frequencies to deallocate by client ~w.~n", [Id]),
            Freqs;  
        _ -> 
            Freq = lists:nth(rand:uniform(Len),Freqs),
            io:format("Frequency ~w deallocated by client ~w.~n", [Freq,Id]),
            frequency_hardened:deallocate(Freq), 
            lists:delete(Freq,Freqs)
    end;
make_request(Id, Freqs, _) ->
    case frequency_hardened:allocate() of
        {ok,Freq} -> 
            io:format("Frequency ~w allocated to client ~w.~n", [Freq,Id]),
            [Freq|Freqs];
        {error, no_frequency} ->
            io:format("No frequencies available for allocation to client ~w~n", [Id]),
            Freqs
    end.

% for debugging purposes: chooses a random element of a non-empty list.
random_elem([]) ->
    empty;
random_elem(Xs) ->
    Len = length(Xs),
    lists:nth(rand:uniform(Len),Xs).  
