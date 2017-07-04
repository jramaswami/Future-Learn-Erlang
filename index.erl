% Future Learn :: Functional Programming in Erlang :: Assignment 2
% @author jramaswami
% A github gist of this is available at:
% https://gist.github.com/jramaswami/9e0b82f77a632f049bff771ebf14a034

-module(index).
-include_lib("eunit/include/eunit.hrl").
-export([index/1, main/1]).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Indexing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Function to call to index given file.
index(FileName) ->
    index_contents(get_file_contents(FileName)).

% Index the contents of the file.
index_contents(Contents) ->
    index_contents(0, filter_stop_words(normalize(tokenize(Contents))), [], []).

% Turn list of string into list of list of string i.e.:
% from ["The first sentence.", "The second sentence."]
% into [["The", "first", "sentence."], ["The", "second", "sentence."].
tokenize(Contents) ->
    [string:tokens(Line, " ") || Line <- Contents].

% Function to convert word to lowercase and remove punctuation with
% exception of apostrophe in a contraction.
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
    Stop = stop_words(),
    filter_stop_words(Stop, Lines).
filter_stop_words(_, []) ->
    [];
filter_stop_words(Stop, [Line|Lines]) ->
    FilterStops = fun(Word) -> length(Word) > 2 andalso (not lists:member(Word, Stop)) end,
    [lists:filter(FilterStops, Line)|filter_stop_words(Stop, Lines)].

stop_words() ->
      ["a", "about", "above", "above", "across", "after", "afterwards", 
       "again", "against", "all", "almost", "alone", "along", "already", 
       "also","although","always","am","among", "amongst", "amoungst", 
       "amount",  "an", "and", "another", "any","anyhow","anyone",
       "anything","anyway", "anywhere", "are", "around", "as",  "at", 
       "back","be","became", "because","become","becomes", "becoming", 
       "been", "before", "beforehand", "behind", "being", "below", "beside", 
       "besides", "between", "beyond", "bill", "both", "bottom","but", "by", 
       "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", 
       "cry", "de", "describe", "detail", "do", "done", "down", "due", 
       "during", "each", "eg", "eight", "either", "eleven","else", 
       "elsewhere", "empty", "enough", "etc", "even", "ever", "every", 
       "everyone", "everything", "everywhere", "except", "few", "fifteen", 
       "fify", "fill", "find", "fire", "first", "five", "for", "former", 
       "formerly", "forty", "found", "four", "from", "front", "full", 
       "further", "get", "give", "go", "had", "has", "hasnt", "have", 
       "he", "hence", "her", "here", "hereafter", "hereby", "herein", 
       "hereupon", "hers", "herself", "him", "himself", "his", "how", 
       "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", 
       "into", "is", "it", "its", "itself", "keep", "last", "latter", 
       "latterly", "least", "less", "ltd", "made", "many", "may", "me", 
       "meanwhile", "might", "mill", "mine", "more", "moreover", "most", 
       "mostly", "move", "much", "must", "my", "myself", "name", "namely", 
       "neither", "never", "nevertheless", "next", "nine", "no", "nobody", 
       "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", 
       "off", "often", "on", "once", "one", "only", "onto", "or", "other", 
       "others", "otherwise", "our", "ours", "ourselves", "out", "over", 
       "own","part", "per", "perhaps", "please", "put", "rather", "re", 
       "same", "see", "seem", "seemed", "seeming", "seems", "serious", 
       "several", "she", "should", "show", "side", "since", "sincere", 
       "six", "sixty", "so", "some", "somehow", "someone", "something", 
       "sometime", "sometimes", "somewhere", "still", "such", "system", 
       "take", "ten", "than", "that", "the", "their", "them", "themselves", 
       "then", "thence", "there", "thereafter", "thereby", "therefore", 
       "therein", "thereupon", "these", "they", "thickv", "thin", "third", 
       "this", "those", "though", "three", "through", "throughout", "thru", 
       "thus", "to", "together", "too", "top", "toward", "towards", "twelve", 
       "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", 
       "via", "was", "we", "well", "were", "what", "whatever", "when", 
       "whence", "whenever", "where", "whereafter", "whereas", "whereby", 
       "wherein", "whereupon", "wherever", "whether", "which", "while", 
       "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", 
       "with", "within", "without", "would", "yet", "you", "your", "yours", 
       "yourself", "yourselves"].

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
insert(LineNumber, Word, [{OtherWord, LineEntries}|Indices]) when Word > OtherWord ->
    [{OtherWord, LineEntries}|insert(LineNumber, Word, Indices)];
insert(LineNumber, Word, [{OtherWord, _LineEntries}|_Indices]=IndexEntries) when Word < OtherWord ->
    [{Word, [{LineNumber, LineNumber}]}|IndexEntries].
    
% Function to reverse all the line numbers in the index because they
% were built in reverse.
reverse_line_number_entries([]) ->
    [];
reverse_line_number_entries([{Word, LineNumbers}|Tail]) ->
    [{Word, lists:reverse(LineNumbers)}|reverse_line_number_entries(Tail)].

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Functions to run from command line:
% erl -noshell -s index main <FileName> -s init stop

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
