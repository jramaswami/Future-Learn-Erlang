% Future Learn :: Concurrent Programming in Erlang :: Frequency Server

-module(frequency).
-export([init/0, start/0, allocate/0, deallocate/1, stop/0]).
-export([overloaded_init/1, overloaded_start/1, allocate/1, deallocate/2]).

% Allocate a frequency, if possible.
alloc({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
alloc({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

% Deallocate frequency for the given Pid only if
% the frequency was allocated to that Pid.
dealloc({Free, Allocated}, {Freq, Pid}) ->
    {NewAllocated, Reply} = dealloc(Allocated, {Freq, Pid}, [], 0),
    case Reply of 
        ok -> {{[Freq|Free], NewAllocated}, Reply};
        no_allocated_match -> {{Free, Allocated}, Reply}
    end.

% Helper function for deallocating frequency.
dealloc([], {_Freq, _Pid}, NewAllocated, N) ->
    case N == 0 of
        true -> {lists:reverse(NewAllocated), no_allocated_match};
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
            {NewFrequencies, Reply} = dealloc(Frequencies, {Freq, Pid}),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid, stop} ->
            Pid ! {reply, stopped}
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
    allocate(500).

allocate(Wait) ->
    clear(),
    frequency ! {request, self(), allocate},
    receive
        {reply, Reply} -> Reply
    after Wait ->
              timeout
    end.

deallocate(Freq) ->
    deallocate(Freq, 500).

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
