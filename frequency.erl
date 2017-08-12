% Future Learn :: Concurrent Programming in Erlang :: Frequency Server

-module(frequency).
-export([init/0, allocate/2, deallocate/2]).

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequence}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, []}, {_Freq, _Pid}) ->
    {{Free, []}, no_deallocation_match};
deallocate({Free, Allocated}, {Freq, Pid}) ->
    deallocate({Free, Allocated}, {Freq, Pid}, 0).
deallocate({Free, []}, {Freq, _Pid}, N) ->
    case N == 0 of
        true -> {, no_deallocation_match};
        false -> {lists:reverse(Acc), ok}
    end;
deallocate({Free, [{Freq, Pid}|Allocated], {Freq, Pid}, N) ->
    deallocate({Freq, Pid}, Allocated, N+1);
deallocate({Freq, Pid}, [{Freq,_P}|Allocated], Acc, N) ->
    deallocate({Freq, Pid}, Allocated, [Freq|Acc], N);
deallocate({Freq, Pid}, [{F,Pid}|Allocated], Acc, N) ->
    deallocate({Freq, Pid}, Allocated, [F|Acc], N);
deallocate({Freq, Pid}, [{F,_P}|Allocated], Acc, N) ->
    deallocate({Freq, Pid}, Allocated, [F|Acc], N).

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            {NewFrequencies, Reply} = deallocate({Freq, Pid}, Frequencies),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid, stop} ->
            Pid ! {reply, stopped}
    end. 

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

get_frequencies() -> [10,11,12,13,14,15].


