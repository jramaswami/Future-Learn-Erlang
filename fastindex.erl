-module(fastindex).
-export([index/1, main/1]).

index(FileName) ->
    {ok, Input} = file:open(FileName, read),
    index_input(Input).

index_input(Input) ->
    reverse_line_entries(
      index_input(Input, io:get_chars(Input, "", 1), 1, [], orddict:new(), load_stop_words())).

index_input(_, eof, LineNumber, Word, Index, Stop) ->
    insert_word(Index, lists:reverse(Word), LineNumber, Stop);
index_input(Input, [Char], LineNumber, Word, Index, Stop) ->
    if 
        Char >= $a andalso Char =< $z ->
            index_input(Input, io:get_chars(Input, "", 1), LineNumber, [Char|Word], Index, Stop);
        Char >= $A andalso Char =< $Z ->
            index_input(Input, io:get_chars(Input, "", 1), LineNumber, [Char+32|Word], Index, Stop);
        Char == $\n ->
            index_input(Input, io:get_chars(Input, "", 1), LineNumber+1, [], insert_word(Index, lists:reverse(Word), LineNumber, Stop), Stop);
        Char == 32  orelse Char == 10 ->
            index_input(Input, io:get_chars(Input, "", 1), LineNumber, [], insert_word(Index, lists:reverse(Word), LineNumber, Stop), Stop);
        Char == $- ->
            index_input(Input, io:get_chars(Input, "", 1), LineNumber, [Char|Word], Index, Stop);
        Char == $' ->
            NextChar = io:get_chars(Input, "", 1),
            case (Char >= $a andalso Char =< $z) orelse (Char >= $A andalso Char =< $Z) of
                true -> index_input(Input, NextChar, LineNumber, [Char|Word], Index, Stop);
                false -> index_input(Input, NextChar, LineNumber, Word, Index, Stop)
            end;
        true ->
            index_input(Input, io:get_chars(Input, "", 1), LineNumber, Word, Index, Stop)
    end.

insert_word(Index, Word, LineNumber, Stop) ->
    case sets:is_element(Word, Stop) orelse length(Word) < 3 of
        true -> Index;
        false ->
            case orddict:find(Word, Index) of
                {ok, [{LineNumber, LineNumber}|_LineEntries]} ->
                    Index;
                {ok, [{Start, End}|LineEntries]} ->
                    case LineNumber - 1 == End of
                        true -> 
                            orddict:store(Word, [{Start,LineNumber}|LineEntries], Index);
                        false -> 
                            orddict:store(Word, [{LineNumber, LineNumber}|[{Start,End}|LineEntries]], Index)
                    end;
                error -> orddict:store(Word, [{LineNumber, LineNumber}], Index)
            end
    end.

reverse_line_entries(Index) ->
    orddict:map(fun(_, LineEntries) -> lists:reverse(LineEntries) end, Index).

load_stop_words() ->
    {ok,File} = file:open("stopwords.txt",[read]),
    sets:from_list(get_all_lines(File,[])).

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

main([FileName]) ->
    try
        Index = index(FileName),
        display_index(Index)
    catch
        _:_ -> usage()
    end;
main(_) -> 
    usage().

usage() ->
    io:format("Usage: escript fastindex.erl <FileName>~n").

display_index(Index) ->
    orddict:map(fun(Word, LineEntries) -> io:format("~s: ~w~n", [Word, LineEntries]), LineEntries end, Index).

