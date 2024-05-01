%%% By Haijun S, Jackson W, and Adnan J. 2024
%%% 
%%% generate_prompts Module
%%% 
-module(generate_prompts).
-export([select_words/2, select_random_elements/2, select_letters/1,select_prompts/2]).

-import(math, [pow/2]).
select_prompts (FilePath, Number) ->
    Words = select_words(FilePath, Number),
    Letters = select_letters(Number),
    Prompts = lists:zip(Letters, Words),
    Prompts.
% Function to read a text file and select a random number of words
select_words(FilePath, NumberOfCategories) ->
    {ok, File} = file:open(FilePath, [read]),
    Words = io:get_line(File, ''),
    WordsList = string:tokens(Words, ",\n"),
    WordCount = length(WordsList),
    Categories = min(NumberOfCategories, WordCount),
    WordsToSelect = select_random_elements(WordsList, Categories),
    file:close(File),
    WordsToSelect.

% Function to randomly select elements from a list
select_random_elements(_, 0) ->
    [];
select_random_elements(List, N) when N > 0 ->
    Index = rand:uniform(length(List)),
    Selected = lists:nth(Index, List),
    Rest = lists:delete(Selected, List),
    [Selected | select_random_elements(Rest, N - 1)].

% Function to randomly choose a number of different letters
select_letters(NumberOfLetters) when is_integer(NumberOfLetters), NumberOfLetters >= 0 ->
    Alphabet = lists:seq($A, $Z), % Generate a list of ASCII values for uppercase letters
    UniqueLettersList = select_unique_elements(Alphabet, NumberOfLetters, []), % Select unique letters
    lists:map(fun(X) -> [X] end, UniqueLettersList).

% Function to select unique elements from a list
select_unique_elements(_, 0, Acc) ->
    Acc;
select_unique_elements(List, N, Acc) when N > 0 ->
    Index = rand:uniform(length(List)),
    {Selected, Rest} = lists:split(Index, List),
    select_unique_elements(Rest ++ tl(Selected), N - 1, [hd(Selected) | Acc]).