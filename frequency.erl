% Future Learn :: Concurrent Programming in Erlang :: Frequency Server

-module(frequency).
-export([init/0, allocate/2, deallocate/2]).

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, {Freq, Pid}) ->
    {NewAllocated, Reply} = dealloc(Allocated, {Freq, Pid}, [], 0),
    case Reply of 
        ok -> {{[Freq|Free], NewAllocated}, Reply};
        no_allocated_match -> {{Free, Allocated}, Reply}
    end.

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

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            {NewFrequencies, Reply} = deallocate(Frequencies, {Freq, Pid}),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid, stop} ->
            Pid ! {reply, stopped}
    end. 

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

get_frequencies() -> [10,11,12,13,14,15].
