% Future Learn Function Programming in Erlang :: Assignment 2
% @author jramaswami

-module(fastindex).
-include_lib("eunit/include/eunit.hrl").
-export([index/1, main/1, display_index/1]).
-export([normalize/1]).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Indexing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Functions for indexing.

% Function to call to index given file.
index(FileName) ->
    index_contents(get_file_contents(FileName)).

% Index the contents of the file.
index_contents(Contents) ->
    index_contents(0, filter_stop_words(normalize(tokenize(Contents))), [], maps:new()).

% Turn list of string into list of list of string i.e.:
% from ["The first sentence.", "The second sentence."]
% into [["The", "first", "sentence."], ["The", "second", "sentence."].
tokenize(Contents) ->
    [string:tokens(Line, " ") || Line <- Contents].

% Function to convert word to lowercase and remove punctuation with
% excpetion of apostrophe in a contraction.
normalize_word([$'|Word]) ->
    normalize_word0(Word);
normalize_word(Word) ->
    normalize_word0(Word).

normalize_word0([]) ->
    [];
normalize_word0([Char|Suffix]) when Char >= $A andalso Char =< $Z ->
    [Char+32|normalize_word0(Suffix)];
normalize_word0([Char|Suffix]) when Char >= $a andalso Char =< $z ->
    [Char|normalize_word0(Suffix)];
normalize_word0([$']) ->
    [];
normalize_word0([$'|Suffix]) ->
    NextChar = hd(Suffix),
    if
        NextChar >= $A andalso NextChar =< $Z -> [$'|normalize_word0(Suffix)];
        NextChar >= $a andalso NextChar =< $z -> [$'|normalize_word0(Suffix)];
        true -> normalize_word0(Suffix)
    end;
normalize_word0([_Char|Suffix]) ->
    normalize_word0(Suffix).

% Convert words into lowercase and remove extraneous punctuation.
normalize([]) ->
    [];
normalize([Line|Lines]) ->
    [[normalize_word(Word) || Word <- Line]|normalize(Lines)].

% Removes words less than length of 3 and any words listed in stopwords.txt.
filter_stop_words(Lines) ->
    Stop = load_stop_words(),
    filter_stop_words(Stop, Lines).
filter_stop_words(_, []) ->
    [];
filter_stop_words(Stop, [Line|Lines]) ->
    FilterStops = fun(Word) -> length(Word) > 2 andalso (not maps:is_key(Word, Stop)) end,
    [lists:filter(FilterStops, Line)|filter_stop_words(Stop, Lines)].

% Load stop words from stopwords.txt into a list.
load_stop_words() ->
    {ok,File} = file:open("stopwords.txt",[read]),
    Stops = get_all_lines(File,[]),
    maps:from_list([{Stop, 1} || Stop <- Stops]).

% Does the actual work of indexing ...
index_contents(_, [], [], Index) ->
    alphabetize_index(reverse_line_number_entries(maps:to_list(Index)));
index_contents(LineNumber, [Line|Lines], [], Index) ->
    index_contents(LineNumber + 1, Lines, Line, Index);
index_contents(LineNumber, Lines, [Word|Words], Index) ->
    index_contents(LineNumber, Lines, Words, insert(LineNumber, Word, Index)).

% If word is in index, add line number to its list of line numbers,
% else add word to index with initial list of line number.
insert(LineNumber, Word, Index) ->
    case maps:is_key(Word, Index) of
        true -> [{Start, End}|LineEntries] = maps:get(Word, Index),
                if
                    LineNumber == End -> Index;
                    LineNumber - 1 == End -> maps:put(Word, [{Start,LineNumber}|LineEntries], Index);
                    true -> maps:put(Word, [{LineNumber,LineNumber}|[{Start,End}|LineEntries]], Index)
                end;
        false -> maps:put(Word, [{LineNumber,LineNumber}], Index)
    end.


% Function to reverse all the line numbers in the index because they
% were built in reverse.
reverse_line_number_entries([]) ->
    [];
reverse_line_number_entries([{Word, LineNumbers}|Tail]) ->
    [{Word, lists:reverse(LineNumbers)}|reverse_line_number_entries(Tail)].

% Function to alphabetize the index.
alphabetize_index(Index) ->
    Compare = fun({Word1, _}, {Word2, _}) -> Word1 < Word2 end,
    lists:sort(Compare, Index).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Template %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Functions provided by instructor.

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
%
% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.
get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.
get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.
show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Functions to run from command line:
% erl -noshell -s fastindex main <FileName> -s init stop

main([FileName]) ->
    try
        Index = index(FileName),
        display_index(Index)
    catch
        _:_ -> usage()
    end;
main(_) -> 
    usage().

display_index(Index) ->
    lists:foreach(fun({Word, LineEntries}) -> io:format("~s: ~w~n", [Word, LineEntries]) end, Index).

usage() ->
    io:format("Usage: escript index.erl <FileName>~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Testing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tokenize_test() ->
    Contents = ["This is a sentence that contains the word sentence.",
                "This sentence also contains that word.",
                "The third sentence does to.",
                "This one does not.  I did not want to include it here.",
                "But this sentence does."
               ],
    Expected = [["This", "is", "a", "sentence", "that", "contains", "the", "word", "sentence."],
                ["This", "sentence", "also", "contains", "that", "word."],
                ["The", "third", "sentence", "does", "to."],
                ["This", "one", "does", "not.", "I", "did", "not", "want", "to", "include", "it", "here."],
                ["But", "this", "sentence", "does."]
               ],
    ?assert(tokenize(Contents) == Expected).

normalize_word_test() ->
    [?assert(normalize_word("HELLO") == "hello"),
     ?assert(normalize_word("'Hello'") == "hello"),
     ?assert(normalize_word("can't'") == "can't"),
     ?assert(normalize_word("Hello!") == "hello")
    ].

normalize_test() ->
    Tokens = [["This", "is", "a", "sentence", "that", "contains", "the", "word", "sentence."],
              ["This", "sentence", "also", "contains", "that", "word."],
              ["The", "third", "sentence", "does", "to."],
              ["This", "one", "does", "not.", "I", "did", "not", "want", "to", "include", "it", "here."],
              ["But", "this", "sentence", "does."]
             ],
    Expected = [["this", "is", "a", "sentence", "that", "contains", "the", "word", "sentence"],
                ["this", "sentence", "also", "contains", "that", "word"],
                ["the", "third", "sentence", "does", "to"],
                ["this", "one", "does", "not", "i", "did", "not", "want", "to", "include", "it", "here"],
                ["but", "this", "sentence", "does"]
               ],
    ?assert(normalize(Tokens) == Expected).

filter_stop_words_test() ->
    Normalized = [["this", "is", "a", "sentence", "that", "contains", "the", "word", "sentence"],
                  ["this", "sentence", "also", "contains", "that", "word"],
                  ["the", "third", "sentence", "does", "to"],
                  ["this", "one", "does", "not", "i", "did", "not", "want", "to", "include", "it", "here"],
                  ["but", "this", "sentence", "does"]
                 ],
    Expected = [["sentence", "contains", "word", "sentence"],
                  ["sentence", "contains", "word"],
                  ["sentence", "does"],
                  ["does", "did", "want", "include"],
                  ["sentence", "does"]
                 ],
    ?assert(filter_stop_words(Normalized) == Expected).
index_contents_test() ->
    Contents = ["This is a sentence that contains the word sentence.",
                "This sentence also contains that word.",
                "The third sentence does to.",
                "This one does not.  I did not want to include it here.",
                "But this sentence does."
               ],
    Expected = [{"contains", [{1,2}]},
                {"did", [{4,4}]},
                {"does", [{3,5}]},
                {"include", [{4,4}]},
                {"sentence", [{1,3}, {5,5}]},
                {"want", [{4,4}]},
                {"word", [{1,2}]}
               ],
    ?assert(index_contents(Contents) == Expected).
