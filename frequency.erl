% Future Learn :: Concurrent Programming in Erlang :: Frequency Server

-module(frequency).
-export([init/0, start/0, allocate/0, deallocate/1, stop/0]).
-export([overloaded_init/1, overloaded_start/1, allocate/1, deallocate/2]).
-export([start_exceptions/0, exceptions_client/1]).

% Functions to test changes for assignment 2.16.
% Just run frequency:start_exceptions().
start_exceptions() ->
    register(frequency, spawn(?MODULE, init, [])),
    register(client, spawn(?MODULE, exceptions_client, [[]])).

exceptions_client([]) ->
    link(whereis(frequency)),
    io:format("Requesting frequency ...~n", []),
    allocate(),
    receive
        {reply, {ok, Freq}} ->
            exceptions_client([Freq])
    end;
exceptions_client([_F|_Freqs]) ->
    io:format("Attempting to deallocate frequency 200 ...~n", []),
    deallocate(200),
    receive 
        {reply, no_allocated_frequency} ->
            io:format("Deallocation failed, as expected.~n", []);
        Reply ->
            io:format("Received unexpected reply ~w~n.", [Reply])
    end,
    io:format("Attempting to send unexpected message ... ~n", []),
    process_flag(trap_exit, true),
    frequency ! {request, spanish_inquisition},
    receive
        {'EXIT', _Pid, Reason} ->
            io:format("Trapped frequency server exit for reason ~w.~n", [Reason]),
            io:format("Restarting frequency server ...~n", []),
            register(frequency, spawn(?MODULE, init, []))
    end,
    io:format("Requesting frequency ...~n", []),
    allocate(),
    io:format("Stopping frequency server ... ~n", []),
    stop(),
    io:format("Stopping client.~n", []).


% Allocate a frequency, if possible.
alloc({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
alloc({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

% Deallocate frequency for the given Pid only if
% the frequency was allocated to that Pid.
dealloc({Free, Allocated}, {Freq, Pid}) ->
    try dealloc(Allocated, {Freq, Pid}, [], 0) of
        {NewAllocated, Reply} -> {{[Freq|Free], NewAllocated}, Reply}
    catch
        throw:no_allocated_frequency -> throw(no_allocated_frequency)
    end.

% Helper function for deallocating frequency.
dealloc([], {_Freq, _Pid}, NewAllocated, N) ->
    case N == 0 of
        true -> throw(no_allocated_frequency);
        false -> {lists:reverse(NewAllocated), ok}
    end;
dealloc([{Freq, Pid}|Allocated], {Freq, Pid}, NewAllocated, N) ->
    dealloc(Allocated, {Freq, Pid}, NewAllocated, N+1);
dealloc([{Freq, P}|Allocated], {Freq, Pid}, NewAllocated, N) ->
    dealloc(Allocated, {Freq, Pid}, [{Freq, P}|NewAllocated], N);
dealloc([{F, Pid}|Allocated], {Freq, Pid}, NewAllocated, N) ->
    dealloc(Allocated, {Freq, Pid}, [{F, Pid}|NewAllocated], N);
dealloc([{F, P}|Allocated], {Freq, Pid}, NewAllocated, N) ->
    dealloc(Allocated, {Freq, Pid}, [{F, P}|NewAllocated], N).

% Event loop.
loop(Frequencies) ->
    io:format("#Log (loop): frequencies = ~w~n", [Frequencies]),
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = alloc(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            try dealloc(Frequencies, {Freq, Pid}) of
                {NewFrequencies, Reply} ->
                    Pid ! {reply, Reply},
                    loop(NewFrequencies)
            catch
                throw:no_allocated_frequency ->
                    io:format("#Log (loop): caught no_allocated_frequency exception.~n", []),
                    Pid ! {reply, no_allocated_frequency},
                    loop(Frequencies)
            end;
        {request, Pid, stop} ->
            Pid ! {reply, stopped};
        _ ->
            io:format("#Log (loop): unexpected message, throwing exception!~n"),
            throw(unexpected_message)
    end. 

% Intialize the event loop with initial set of frequencies.
init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

% Retrieve initial frequencies.
get_frequencies() -> [10,11,12,13,14,15].

% Registers and spawns frequency server.
start() ->
    register(frequency, spawn(?MODULE, init, [])).

% High level API
allocate() ->
    allocate(0).

allocate(Wait) ->
    clear(),
    frequency ! {request, self(), allocate},
    receive
        {reply, Reply} -> Reply
    after Wait ->
              timeout
    end.

deallocate(Freq) ->
    deallocate(Freq, 0).

deallocate(Freq, Wait) ->
    clear(),
    frequency ! {request, self(), {deallocate, Freq}},
    receive
        {reply, Reply} -> Reply
    after Wait ->
              timeout
    end.

stop() ->
    clear(),
    frequency ! {request, self(), stop},
    receive
        {reply, Reply} -> Reply
    end.

% Clear mailbox (with non-blocking timeout).
clear() ->
    receive Msg -> 
                io:format("#Log (clear): Clearing ~w~n", [Msg]), 
                clear()
    after 0 -> 
              ok
    end.

% "Overloaded server " event loop.
overloaded_loop(Frequencies, Delay) ->
    io:format("#Log (overloaded loop [~w]): frequencies = ~w~n", [Delay,Frequencies]),
    receive
        {request, Pid, allocate} ->
            timer:sleep(Delay),
            {NewFrequencies, Reply} = alloc(Frequencies, Pid),
            Pid ! {reply, Reply},
            overloaded_loop(NewFrequencies, Delay);
        {request, Pid, {deallocate, Freq}} ->
            timer:sleep(Delay),
            {NewFrequencies, Reply} = dealloc(Frequencies, {Freq, Pid}),
            Pid ! {reply, Reply},
            overloaded_loop(NewFrequencies, Delay);
        {request, Pid, stop} ->
            Pid ! {reply, stopped}
    end. 

% Intialize the "overloaded server" event loop with initial set of frequencies.
overloaded_init(Delay) ->
    Frequencies = {get_frequencies(), []},
    overloaded_loop(Frequencies, Delay).

% Registers and spawns frequency server.
overloaded_start(Delay) ->
    register(frequency, spawn(?MODULE, overloaded_init, [Delay])).
