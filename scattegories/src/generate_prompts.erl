%%% By Haijun S, Jackson W, and Adnan J. 2024
%%% 
%%% generate_prompts Module
%%% 
-module(generate_prompts).
-export([select_words/2, select_random_elements/2, select_letters/1,
         select_prompts/2]).

-import(math, [pow/2]).

%% select_prompts (FilePath, Number): Returns the categories and letters 
%% for each game
select_prompts (FilePath, Number) ->
    Words = select_words(FilePath, Number),
    Letters = select_letters(Number),
    Prompts = lists:zip(Letters, Words),
    Prompts.
%% select_words(FilePath, NumberOfCategories): Returns the categories for each 
%% game
select_words(FilePath, NumberOfCategories) ->
    {ok, File} = file:open(FilePath, [read]),
    Words = io:get_line(File, ''),
    WordsList = string:tokens(Words, ",\n"),
    WordCount = length(WordsList),
    Categories = min(NumberOfCategories, WordCount),
    WordsToSelect = select_random_elements(WordsList, Categories),
    file:close(File),
    WordsToSelect.

%% select_random_elements(List, N): Returns a list of random elements
%% given a list and number of elements to choose
select_random_elements(_, 0) ->
    [];
select_random_elements(List, N) when N > 0 ->
    Index = rand:uniform(length(List)),
    Selected = lists:nth(Index, List),
    Rest = lists:delete(Selected, List),
    [Selected | select_random_elements(Rest, N - 1)].

%% select_letters(NumberOfLetters): Returns a list of letters given the 
%% number of letters to choose
select_letters(NumberOfLetters)
when is_integer(NumberOfLetters), NumberOfLetters >= 0 ->
    % Generate a list of ASCII values for uppercase letters
    Alphabet = lists:seq($A, $Z),
    UniqueLettersList = select_unique_elements(Alphabet, NumberOfLetters, []),
    lists:map(fun(X) -> [X] end, UniqueLettersList).

%% select_unique_elements(List, N, Acc): Returns a list of unique elements 
%% given a list and number of elements to vhoosr 
select_unique_elements(_, 0, Acc) ->
    Acc;
select_unique_elements(List, N, Acc) when N > 0 ->
    Index = rand:uniform(length(List)),
    {Selected, Rest} = lists:split(Index, List),
    select_unique_elements(Rest ++ tl(Selected), N - 1, [hd(Selected) | Acc]).
