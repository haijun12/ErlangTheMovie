-module(p).
-behaviour(gen_server).

-export([create/3, join/3, list_games/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).

%% -define(DEBUG(Format, Args), io:format("[DEBUG] " ++ Format, Args)).
-define(DEBUG(Format, Args), void).

%%============================================================================%%
%%============================================================================%%
%%============================================================================%%
%% Client API
%%============================================================================%%
%%============================================================================%%
%%============================================================================%%

setup(GameState) ->
    erlang:set_cookie(?COOKIE),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GameState], []),
    ok.

setup2() ->
    send_messages(),
    gen_server:call(?SERVER, {clientleave}),
    ok.

create(Username, GameName, Network) ->
    GameState = game:init(GameName, Network),
    MePeer = game:init_peer(node(), Username)
    GameState2 = game:add_player(MePeer, GameState)
    setup(GameState2),
    %% gen_server:cast({?DSERVER, NetworkName}, {add, node(), GameName}),
    setup2(Name).

join(Username, Node, Network) ->
    MePeer = game:init_peer(node(), Username),
    setup({joining, MePeer, Network}),
    ok = gen_server:call(?SERVER, {clientjoin, Node}),
    setup2().

list_games(Network) ->
    PeerMap = gen_server:call({?DSERVER, Network}, {list}),
    io:format("~p~n", [PeerMap]).


%%============================================================================%%
%%============================================================================%%
%%============================================================================%%
%% gen_server
%%============================================================================%%
%%============================================================================%%
%%============================================================================%%

init([Username, NetworkName]) ->
    {ok, game:init(Username, NetworkName

handle_cast(_Request, State) ->
    {noreply, State}.

%%============================================================================%%
%% joining
%%============================================================================%%
handle_call({clientjoin, JoinNode}, _From, {joining, MePeer, Network}) ->
    ?DEBUG("handle_call clientjoin~n", []),
    
    {ok, GameState} = gen_server:call({?SERVER, Peer}, {join, MePeer}),
    %% gen_server:cast({?DSERVER, NetworkName}, {add, node(), GameName}),
    {reply, ok, GameState}

handle_call({join, JoiningPeer}, _From, GameState) ->
    %% TODO ensure game hasn't started yet
    
    ?DEBUG("handle_call join~n", []),
    cast_to_peers({newpeer, JoiningPeer}, GameState),
    NewGameState = game:add_player(JoiningPeer, GameState),
    {reply, {ok, NewGameState}, NewGameState};

handle_call({newpeer, JoiningPeer}, _From, GameState) ->
    ?DEBUG("handle_call newpeer~n", []),
    NewGameState = game:add_player(JoiningPeer, GameState),
    {reply, ok, NewGameState}

%%============================================================================%%
%% messages
%%============================================================================%%

handle_call({clientsend, Message}, _From, GameState) ->
    ?DEBUG("handle_call clientsend~n", []),
    cast_to_peers({message, Message, Name, Peers, node()}, Peers),
    ?DEBUG("~p~n", [ChatHistory]),
    {reply, ok, State#state{chat_history=[{Name, Message} | ChatHistory]}};

handle_call({message, Message, Fromname, Frompeerlist, Frompeer}, _From,
            State=#state{chat_history=ChatHistory, peers=Peers}) ->
    ?DEBUG("handle_call message~n", []),
    Peerlist = [Frompeer | lists:delete(node(), Frompeerlist)],
    Pruned = lists:subtract(Peers, Peerlist),
    cast_to_peers({message, Message, Fromname, Peers, node()}, Pruned),
    print_message({Fromname, Message}),
    {reply, ok, State#state{chat_history=[{Fromname, Message} | ChatHistory]}};

%%============================================================================%%
%% leave
%%============================================================================%%

handle_call({clientleave}, _From,
            State=#state{peers=Peers, network=NetworkName}) ->
    ?DEBUG("handle_call clientleave~n", []),
    gen_server:cast({?DSERVER, NetworkName}, {delete, node()}),
    cast_to_peers({leave, node()}, Peers),
    {stop, normal, ok, State#state{peers=[], chat_history=[]}};

handle_call({leave, Peer}, _From, State=#state{peers=Peers}) ->
    ?DEBUG("handle_call leave~n", []),
    NewPeers = lists:delete(Peer, Peers),
    {reply, ok, State#state{peers=NewPeers}};

%%============================================================================%%
%% list
%%============================================================================%%

handle_call({list}, _From, State=#state{network=NetworkName}) ->
    ?DEBUG("handle_call list~n", []),
    PeerMap = gen_server:call({?DSERVER, NetworkName}, {list}),
    io:format("~p~n", [PeerMap]),
    {reply, ok, State}.

%%============================================================================%%
%% 
%%============================================================================%%

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%============================================================================%%
%%============================================================================%%
%%============================================================================%%
%% Helpers
%%============================================================================%%
%%============================================================================%%
%%============================================================================%%

cast_to_peers(Cast, Peers) ->
    util:pmap(fun (Peer) -> gen_server:call({?SERVER, Peer}, Cast) end, Peers).

send_messages(Name) ->
    case io:get_line(Name ++ ": ") of
        "--leave\n" ->
            ok;
        "--list\n" ->
            gen_server:call(?SERVER, {list}),
            send_messages(Name);
        Message ->
            gen_server:call(?SERVER, {clientsend, Message}),
            send_messages(Name)
    end.

print_message({Name, Message}) ->
    io:format("~s: ~s", [Name, Message]).
