-module(game).

-export([init/0, add_player/2, change_round/1, add_data/2]).

-record (gameState, {numPlayers = 0, round = -1, players_ready = 0, prompts = [], letters = [], leaderboard = [], round_data = []}).

-define(ENDROUND, 2).
-define(DEBUG(Format, Args), void).

init() ->
    % generate prompts with parameter ENDROUND
    {ok, #gameState{numPlayers = 0, letters = ["A"], prompts= ["An animal"]}}.

add_player(playerName, #gameState{numPlayers = numPlayers, leaderboard = board}) ->
    ?DEBUG("Adding a new player ~n", []),
    % Assumes round hasn't started
    {ok, #gameState{numPlayers = numPlayers + 1, leaderboard = [{playerName, 0} | board]}}.


add_data(playerData, #gameState{players_ready = players_ready, leaderboard = Board, round_data = Data}) ->
    ?DEBUG("Player submitted ~n", [playerData]),
    #gameState{players_ready = players_ready + 1, leaderboard = Board, round_data = [playerData | Data]}.

change_round(#gameState{round = Round, leaderboard = Board}) when Round == -1 ->
    ?DEBUG("Game Starting ~n", []),
    % Handle Starting Game
    {game_started, #gameState{round = Round + 1, players_ready = 0, leaderboard = Board, round_data = []}};

change_round(#gameState{round = Round, leaderboard = Board, round_data = Data}) when Round ==  ?ENDROUND ->
    ?DEBUG("Game Ending ~n", []),
    % Handle Ending Game
    {game_over, #gameState{round = Round + 1, leaderboard = Board, round_data = Data}};

change_round(#gameState{round = Round, leaderboard = Board, round_data = Data}) when Round rem 2 == 0 ->
    ?DEBUG("Prompt round starting ~n", []),
    % Handle Prompt rounds
    {new_round, #gameState{round = Round + 1, leaderboard = Board, round_data = Data}};

change_round(#gameState{round = Round, leaderboard = Board, round_data = Data}) when Round rem 2 == 1 ->
    ?DEBUG("Voting round starting ~n", []),
    % Handle Voting rounds
    % Update the game state or perform specific actions
    {voting_round, #gameState{round = Round + 1, leaderboard = Board, round_data = Data}}.





    


