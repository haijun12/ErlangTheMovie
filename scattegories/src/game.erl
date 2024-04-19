-module(game).

-export([init/2, init_peer/2, add_player/2, client_input/2, peer_input/3]).

-record (peer, {node, name}).
-record (game_state, {round = -1, prompts = [], letters = [], peers = [], round_data = [], game_name, network}).

-define(ENDROUND, 2).
-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).

-define(DEBUG(Format, Args), io:format("[DEBUG] [game.erl] " ++ Format, Args)).
%% -define(DEBUG(Format, Args), void).

init(GameName, Network) ->
    #game_state{letters = ["A"], prompts= ["An animal"], game_name=GameName, network = Network}.

init_peer(Node, Name) ->
    #peer{node=Node, name=Name}.


add_player(JoiningPeer, GameState=#game_state{peers = Peers}) ->
    ?DEBUG("Adding a new player ~n", []),
    % Assumes round hasn't started
    update_peers({alert_new_peer, JoiningPeer}, GameState),
    GameState#game_state{peers = [JoiningPeer | Peers]}.

client_input(voteready, GameState=#game_state{peers=_Peers, round=-1}) ->
    update_peers(voteready, GameState),
    %% TODO update gamestate so we are ready
    GameState;

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
    MePeer = get_me_peer(Peers),
    PeersSansMePeer = lists:delete(MePeer, Peers),
    util:pmap(fun (#peer{node=PeerNode}) -> gen_server:call({?SERVER, PeerNode}, {peerinput, Action, MePeer}) end, PeersSansMePeer).

get_me_peer([MePeer=#peer{node=Node} | _T]) when Node == node() ->
    MePeer;
get_me_peer([_H | T]) ->
    get_me_peer(T);
get_me_peer([]) ->
    error.

print_game_state(#game_state{round=Round}) ->
    ?DEBUG("round: ~p~n", [Round]).
