-module(game).

-export([init/2, add_player/2, client_input/2, peer_input/3]).

-record (game_state, {round = -1, prompts = [], letters = [], peers = [], game_name, network}).

-define(ENDROUND, 2).
-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).

-define(DEBUG(Format, Args), io:format("[DEBUG] [game.erl] " ++ Format, Args)).
%% -define(DEBUG(Format, Args), void).

init(GameName, Network) ->
    #game_state{letters = ["A"], prompts= ["An animal"], game_name=GameName, network = Network}.



add_player(JoiningPeer, GameState=#game_state{peers = Peers}) ->
    ?DEBUG("Adding a new player ~n", []),
    % Assumes round hasn't started
    update_peers({alert_new_peer, JoiningPeer}, GameState),
    GameState#game_state{peers = [JoiningPeer | Peers]}.

client_input(voteready, GameState=#game_state{peers=Peers, round=-1}) ->
    update_peers(voteready, GameState),
    MePeer = gamepeer:get_me_peer(Peers),
    NewPeers = gamepeer:set_peer_data(ready, MePeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

client_input(Action, GameState) ->
    io:format("Unrecognized client action~n", []),
    ?DEBUG("Action was ~p~n", [Action]),
    GameState.

peer_input(voteready, FromPeer, GameState=#game_state{peers=Peers, round=-1}) ->
    NewPeers = gamepeer:set_peer_data(ready, FromPeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

peer_input(update_game, _FromPeer, GameState) ->
    GameState;

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
        io:format("All players are ready, advancing to ~p", [Round + 1]),
        NewPeers = gamepeer:set_all_data(not_ready, Peers),
        GameState#game_state{peers=NewPeers, round=Round + 1};
    _ ->
        GameState
    end.
