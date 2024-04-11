-module(p).
-behaviour(gen_server).

-export([create/3, join/3, list_games/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {name, peers, gamestate, chat_history = [], network}).

-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).

%% -define(DEBUG(Format, Args), io:format("[DEBUG] " ++ Format, Args)).
-define(DEBUG(Format, Args), void).

%%============================================================================%%
%% Client API
%%============================================================================%%

setup(Name, GameState, NetworkName) ->
    erlang:set_cookie(?COOKIE),
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [Name, GameState, NetworkName], []),
    ok.

setup2(Name) ->
    send_messages(Name),
    gen_server:call(?SERVER, {clientleave}),
    ok.

create(Name, GameName, NetworkName) ->
    setup(Name, {inroom, GameName}, NetworkName),
    gen_server:cast({?DSERVER, NetworkName}, {add, node(), GameName}),
    setup2(Name).

join(Name, Node, NetworkName) ->
    setup(Name, {noroom}, NetworkName),
    {ok, GameName} = gen_server:call(?SERVER, {clientjoin, Node}),
    gen_server:cast({?DSERVER, NetworkName}, {add, node(), GameName}),
    setup2(Name).

list_games (NetworkName) ->
    PeerMap = gen_server:call({?DSERVER, NetworkName}, {list}),
    io:format("~p~n", [PeerMap]).


%%============================================================================%%
%% gen_server
%%============================================================================%%

init([Name, GameState, NetworkName]) ->
    {ok, #state{name=Name, peers=[], gamestate=GameState, network=NetworkName}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call({clientjoin, Peer}, _From, State) ->
    ?DEBUG("handle_call clientjoin~n", []),
    {ok, NewPeers, GameName, NewChatHistory} = gen_server:call({?SERVER, Peer}, {join, node()}),
    lists:map(fun (HistMsg) -> print_message(HistMsg) end,
              lists:reverse(NewChatHistory)),
    {reply, {ok, GameName}, State#state{peers=[Peer | NewPeers],
                                        gamestate={inroom, GameName},
                                        chat_history=NewChatHistory}};

handle_call({join, Peer}, _From,
            State=#state{peers=Peers,
                         gamestate={inroom, GameName},
                         chat_history=ChatHistory}) ->
    ?DEBUG("handle_call join~n", []),
    cast_to_peers({newpeer, Peer}, Peers),
    {reply, {ok, Peers, GameName, ChatHistory},
     State#state{peers=[Peer | Peers]}};

handle_call({clientsend, Message}, _From,
            State=#state{name=Name,
                         peers=Peers,
                         chat_history=ChatHistory}) ->
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

handle_call({newpeer, Peer}, _From, State=#state{peers=Peers}) ->
    ?DEBUG("handle_call newpeer~n", []),
    {reply, ok, State#state{peers=[Peer | Peers]}};

handle_call({list}, _From, State=#state{network=NetworkName}) ->
    ?DEBUG("handle_call list~n", []),
    PeerMap = gen_server:call({?DSERVER, NetworkName}, {list}),
    io:format("~p~n", [PeerMap]),
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%============================================================================%%
%% Helpers
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
