-module(game).

-export([init/2, add_player/2, client_input/2, peer_input/3]).

-record (game_state, {round = -1, prompts = [], peers = [], game_name, network}).

-define(ENDROUND, 2).
-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).

-define(DEBUG(Format, Args), io:format("[DEBUG] [game.erl] " ++ Format, Args)).
%% -define(DEBUG(Format, Args), void).

init(GameName, Network) ->
    #game_state{prompts= [{"A", "An animal"}, {"B", "A fruit"}], game_name=GameName, network=Network}.

add_player(JoiningPeer, GameState=#game_state{peers = Peers}) ->
    ?DEBUG("Adding a new player ~n", []),
    ?DEBUG("New peer: ~p~n", [JoiningPeer]),
    ?DEBUG("Peer list: ~p~n", [Peers]),
    % Assumes round hasn't started
    update_peers({alert_new_peer, JoiningPeer}, GameState),
    GameState#game_state{peers = [JoiningPeer | Peers]}.

client_input("ready", GameState=#game_state{peers=Peers, round=-1}) ->
    update_peers(voteready, GameState),
    MePeer = gamepeer:get_me_peer(Peers),
    NewPeers = gamepeer:set_peer_data(ready, MePeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

% Add leaving

client_input(Input, GameState=#game_state{peers=Peers, round=Round}) when Round rem 2 == 0 ->
    update_peers({submit, Input}, GameState),
    MePeer = gamepeer:get_me_peer(Peers),
    NewPeers = gamepeer:set_peer_data(Input, MePeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

client_input(Input, GameState=#game_state{peers=Peers, round=Round}) when Round rem 2 == 1 ->
    case gamepeer:check_valid_vote(Input, Peers) of true ->
        update_peers({vote, Input}, GameState),
        MePeer = gamepeer:get_me_peer(Peers),
        NewPeers = gamepeer:set_peer_data(Input, MePeer, Peers),
        NewPeers2 = gamepeer:add_peer_points(Input, NewPeers),
        NewGameState = GameState#game_state{peers=NewPeers2},
        advance_if_all_ready(NewGameState);
    _ ->
        io:format("You cannot vote for yourself!~n", []),
        GameState
    end;

client_input(Action, GameState) ->
    io:format("Unrecognized client action~n", []),
    ?DEBUG("Action was ~p~n", [Action]),
    GameState.

peer_input(voteready, FromPeer, GameState=#game_state{peers=Peers, round=-1}) ->
    NewPeers = gamepeer:set_peer_data(ready, FromPeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

peer_input({submit, Input}, FromPeer, GameState=#game_state{peers=Peers, round=Round}) when Round rem 2 == 0 ->
    NewPeers = gamepeer:set_peer_data(Input, FromPeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

peer_input({vote, Input}, FromPeer, GameState=#game_state{peers=Peers, round=Round}) when Round rem 2 == 1 ->
    NewPeers = gamepeer:set_peer_data(Input, FromPeer, Peers),
    % Update peers points
    NewPeers2 = gamepeer:add_peer_points(Input, NewPeers),
    NewGameState = GameState#game_state{peers=NewPeers2},
    advance_if_all_ready(NewGameState);

peer_input({alert_new_peer, JoiningPeer}, _FromPeer, GameState=#game_state{peers=Peers}) ->
    print_game_state(GameState),
    GameState#game_state{peers=[JoiningPeer | Peers]};

peer_input(Action, FromPeer, GameState) ->
    io:format("Unrecognized peer action~n", []),
    ?DEBUG("Action was ~p~n", [Action]),
    ?DEBUG("FromPeer was ~p~n", [FromPeer]),
    GameState.


%%============================================================================%%
%% non-exported helpers
%%============================================================================%%

update_peers(_, #game_state{peers=[]}) ->
    %% this will only happen at the beginning of the game when the first user
    %% adds themselves
    [ok];
update_peers(Action, #game_state{peers=Peers}) ->
    MePeer = gamepeer:get_me_peer(Peers),
    PeersSansMePeer = lists:delete(MePeer, Peers),
    PeerNodes = gamepeer:get_peer_nodes(PeersSansMePeer),
    util:pmap(fun (PeerNode) -> gen_server:call({?SERVER, PeerNode}, {peerinput, Action, MePeer}) end, PeerNodes).

print_game_state(#game_state{round=Round}) ->
    ?DEBUG("round: ~p~n", [Round]).

advance_if_all_ready(GameState=#game_state{peers=Peers, round=Round}) ->
    PeersData = gamepeer:get_data(Peers),
    IsReady = lists:foldl(fun(Data, Accum) ->
                              case Data of
                                  not_ready -> not_ready;
                                  _ -> Accum
                              end
                          end, ready, PeersData),
    case IsReady of ready ->
        io:format("All players are ready, advancing to ~p~n", [Round + 1]),
        NewPeers = gamepeer:shift_data(Peers),
        NewPeers2 = gamepeer:set_all_data(not_ready, NewPeers),
        NewGameState = GameState#game_state{peers=NewPeers2, round=Round + 1},
        print_round_prompt(NewGameState),
        shift_prompts(NewGameState);
    _ ->
        % I'd prefer it to show the number of players ready
        io:format("Not all players ready yet ~n", []),
        GameState
    end.

print_round_prompt(#game_state{round=Round, prompts=[{Letter, Prompt} | _T], peers=Peers}) when Round rem 2 == 0 ->
    % Show score here
    LeaderBoard = gamepeer:get_current_points(Peers),
    io:format("Current Leaderboard:~n", []),
    lists:map(fun ({Username, Points}) -> io:format("~s: ~p~n", [Username, Points]) end, LeaderBoard),
    % Print prompts for next round
    io:format("~nPrompt: ~s, Letter: ~s~n", [Prompt, Letter]),
    io:format("Enter your answer:~n");

print_round_prompt(#game_state{round=Round, prompts=[{Letter, Prompt} | _T], peers=Peers}) when Round rem 2 == 1 ->
    % Print prompts for next round
    io:format("~nPrompt: ~s, Letter: ~s~n", [Prompt, Letter]),
    io:format("Responses:~n", []),
    UsernameData = gamepeer:get_username_data_old(Peers),
    lists:map(fun ({Username, Data}) -> io:format("~s: ~s~n", [Username, Data]) end, UsernameData),
    io:format("Vote by username:~n");

print_round_prompt(#game_state{round=Round, prompts=[], peers=Peers}) when Round rem 2 == 0 ->
    io:format("Game Over!~n", []),
    % Show score here
    LeaderBoard = gamepeer:get_current_points(Peers),
    io:format("Current Leaderboard:~n", []),
    lists:map(fun ({Username, Points}) -> io:format("~s: ~p~n", [Username, Points]) end, LeaderBoard).

shift_prompts(GameState=#game_state{round=Round}) when Round rem 2 == 0 ->
    GameState;

shift_prompts(#game_state{round=Round, prompts=[]}) when Round rem 2 == 0 ->
    % On leaving game, we also have to kill the input
    #game_state{};

shift_prompts(GameState=#game_state{round=Round, prompts=[_P | Prompts]}) when Round rem 2 == 1 ->
    GameState#game_state{prompts=Prompts}.

print(Text) ->
    io:format("SCATTERGORIES # ~p~n", [Text]).