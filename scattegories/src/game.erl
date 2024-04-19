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
    MePeer = peer:get_me_peer(Peers),
    NewPeers = peer:set_peer_data(ready, MePeer, Peers),
    PeersData = peer:get_data(NewPeers),
    is_ready=lists:foldl(fun (Data, Accum) -> case Data of not_ready -> not_ready; _ -> Accum end, ready, PeersData),
    case is_ready of ready ->
        %% reset ALL peer data fields to be SOMETHING
        %% update round number (to concrete number)
        NewGameState = GameState
    _ ->
        NewGameState = GameState#{peers=NewPeers};
    NewGameState.

client_input(Action, GameState) ->
    io:format("Unrecognized client action~n", []),
    ?DEBUG("Action was ~p~n", [Action]),
    GameState.

peer_input(voteready, _FromPeer, GameState) ->
    %% update gamestate so peer is ready
    print_game_state(GameState),
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

update_peers(Action, #game_state{peers=Peers}) ->
    MePeer = peer:get_me_peer(Peers),
    PeersSansMePeer = lists:delete(MePeer, Peers),
    PeerNodes = peer:get_peer_nodes(PeersSansMePeer),
    util:pmap(fun (PeerNode) -> gen_server:call({?SERVER, PeerNode}, {peerinput, Action, MePeer}) end, PeerNodes).


print_game_state(#game_state{round=Round}) ->
    ?DEBUG("round: ~p~n", [Round]).
