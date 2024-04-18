-module(game).

-export([init/2, init_peer/2, add_player/2, change_round/1, add_data/2]).

-record (peer, {node, username}).
-record (game_state, {round = -1, prompts = [], letters = [], peers = [], round_data = [], game_name, network}).

-define(ENDROUND, 2).
-define(DEBUG(Format, Args), void).

init(GameName, Network) ->
    % generate prompts with parameter ENDROUND
    {ok, #game_state{letters = ["A"], prompts= ["An animal"], game_name=GameName, network = Network}}.

init_peer(Node, Username) ->
    #peer{node=Node, username=Username}.

add_player(Peer, GameState = #game_state{peers = Peers}) ->

    ?DEBUG("Adding a new player ~n", []),
    % Assumes round hasn't started
    {ok, GameState#game_state{peers = [Peer | Peers]}}.


add_data(Player_Data, #game_state{players_ready = Players_Ready, leaderboard = Board, round_data = Data}) ->
    ?DEBUG("Player submitted ~n", [Player_Data]),
    #game_state{players_ready = Players_Ready + 1, leaderboard = Board, round_data = [Player_Data | Data]}.

change_round(#game_state{round = Round, leaderboard = Board}) when Round == -1 ->
    ?DEBUG("Game Starting ~n", []),
    % Handle Starting Game
    {game_started, #game_state{round = Round + 1, players_ready = 0, leaderboard = Board, round_data = []}};

change_round(#game_state{round = Round, leaderboard = Board, round_data = Data}) when Round ==  ?ENDROUND ->
    ?DEBUG("Game Ending ~n", []),
    % Handle Ending Game
    {game_over, #game_state{round = Round + 1, leaderboard = Board, round_data = Data}};

change_round(#game_state{round = Round, leaderboard = Board, round_data = Data}) when Round rem 2 == 0 ->
    ?DEBUG("Prompt round starting ~n", []),
    % Handle Prompt rounds
    {new_round, #game_state{round = Round + 1, leaderboard = Board, round_data = Data}};

change_round(#game_state{round = Round, leaderboard = Board, round_data = Data}) when Round rem 2 == 1 ->
    ?DEBUG("Voting round starting ~n", []),
    % Handle Voting rounds
    % Update the game state or perform specific actions
    {voting_round, #game_state{round = Round + 1, leaderboard = Board, round_data = Data}}.





    


