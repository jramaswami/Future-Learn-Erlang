% Future Learn Function Programming in Erlang :: Assignment 2
% @author jramaswami

-module(index).
-include_lib("eunit/include/eunit.hrl").
-export([index/1, main/1, display_index/1]).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Indexing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Functions for indexing.

% Function to call to index given file.
index(FileName) ->
    index_contents(get_file_contents(FileName)).

% Index the contents of the file.
index_contents(Contents) ->
    alphabetize_index(index_contents(0, filter_stop_words(normalize(tokenize(Contents))), [], [])).

% Turn list of string into list of list of string i.e.:
% from ["The first sentence.", "The second sentence."]
% into [["The", "first", "sentence."], ["The", "second", "sentence."].
tokenize(Contents) ->
    lists:map(fun(Line) -> string:tokens(Line, " ") end, Contents).

% Convert words into lowercase and remove extraneous punctuation.
normalize([]) ->
    [];
normalize([Line|Lines]) ->
    Punctuation = [33, 34, 35, 36, 37, 38, 40, 41, 42, 43, 44, 46, 47,
                   58, 59, 60, 61, 62, 63, 64, 91, 92, 93, 94, 96],
    RemovePunctuation = fun(Char) -> not lists:member(Char, Punctuation) end,
    NormalizeWord = fun(Word) -> string:to_lower(lists:filter(RemovePunctuation, Word)) end,
    [lists:map(NormalizeWord, Line)|normalize(Lines)].

% Removes words less than length of 3 and any words listed in stopwords.txt.
filter_stop_words(Lines) ->
    Stop = load_stop_words(),
    filter_stop_words(Stop, Lines).
filter_stop_words(_, []) ->
    [];
filter_stop_words(Stop, [Line|Lines]) ->
    FilterStops = fun(Word) -> length(Word) > 2 andalso (not lists:member(Word, Stop)) end,
    [lists:filter(FilterStops, Line)|filter_stop_words(Stop, Lines)].

% Load stop words from stopwords.txt into a list.
load_stop_words() ->
    {ok,File} = file:open("stopwords.txt",[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Does the actual work of indexing ...
index_contents(_, [], [], Acc) ->
    reverse_line_number_entries(Acc);
index_contents(LineNumber, [Line|Lines], [], Acc) ->
    index_contents(LineNumber + 1, Lines, Line, Acc);
index_contents(LineNumber, Lines, [Word|Words], Acc) ->
    index_contents(LineNumber, Lines, Words, insert(LineNumber, Word, Acc)).

% If word is in index, add line number to its list of line numbers,
% else add word to index with initial list of line number.
insert(LineNumber, Word, []) ->
    [{Word, [{LineNumber, LineNumber}]}];
insert(LineNumber, Word, [{Word, [{Start,LineNumber}|LineEntries]}|Indices]) ->
    [{Word, [{Start,LineNumber}|LineEntries]}|Indices];
insert(LineNumber, Word, [{Word, [{Start,End}|LineEntries]}|Indices]) ->
    case LineNumber - 1 == End of
        true -> [{Word, [{Start,LineNumber}|LineEntries]}|Indices];
        false -> [{Word, [{LineNumber, LineNumber}|[{Start,End}|LineEntries]]}|Indices]
    end;
insert(LineNumber, Word, [{OtherWord, LineEntries}|Indices]) ->
    [{OtherWord, LineEntries}|insert(LineNumber, Word, Indices)].
    
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

% Functions to run as escript

main([FileName]) ->
    try
        io:format("Indexing ~s ... ~n", [FileName]),
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