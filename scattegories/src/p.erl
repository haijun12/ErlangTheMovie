-module(p).
-behaviour(gen_server).

-export([create/2, join/2, games/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {name, peers, gamestate, chat_history = []}).

-define (COOKIE, scattegories).
-define (SERVER, scattegories).

-define(DEBUG(Format, Args), io:format("[DEBUG] " ++ Format, Args)).
%% -define(DEBUG(Format, Args), void).

%%============================================================================%%
%% Client API
%%============================================================================%%

setup(Name, GameState) ->
    erlang:set_cookie(?COOKIE),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Name, GameState], []),
    ok.

setup2() ->
    send_messages(),
    gen_server:call(?SERVER, {clientleave}),
    ok.

create(Name, GameName) ->
    setup(Name, {inroom, GameName}),
    p_network:update_network({add, node(), GameName}),
    setup2().

join(Name, Node) ->
    setup(Name, {noroom}),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Name], []),
    {ok, GameName} = gen_server:call(?SERVER, {clientjoin, Node}),
    p_network:update_network({add, node(), GameName}),
    setup2().

games () ->
    p_network:update_network(list).


%%============================================================================%%
%% gen_server
%%============================================================================%%

init([Name, GameState]) ->
    {ok, #state{name=Name, peers=[], gamestate=GameState}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call({clientjoin, Peer}, _From, State) ->
    ?DEBUG("handle_call clientjoin~n", []),
    {ok, NewPeers, GameName, ChatHistory} = gen_server:call({?SERVER, Peer}, {join, node()}),
    io:format("Chat History: ~p~n", [State#state.chat_history]),
    {reply, {ok, GameName}, State#state{peers=[Peer | NewPeers], chat_history=ChatHistory}};

handle_call({join, Peer, ChatHistory}, _From,
            State=#state{peers=Peers,
                         gamestate={inroom, GameName},
                         chat_history=ChatHistory}) ->
    ?DEBUG("handle_call join~n", []),
    cast_to_peers({newpeer, Peer}, Peers),
    {reply, {ok, Peers, GameName, ChatHistory},
     State#state{peers=[Peer | Peers]}};

handle_call({clientsend, Message}, _From,
            State=#state{name=Name, peers=Peers}) ->
    ?DEBUG("handle_call clientsend~n", []),
    NewChatHistory = State#state.chat_history ++ [{from = Name, message = Message}], % Append message to chat history
    cast_to_peers({message, Message, Name}, Peers),
    {reply, ok, State#state{chat_history = NewChatHistory}};

handle_call({message, Message, Fromname}, _From, State) ->
    ?DEBUG("handle_call message~n", []),
    io:format("~s: ~s", [Fromname, Message]),
    {reply, ok, State};

handle_call({clientleave}, _From, State=#state{peers=Peers}) ->
    ?DEBUG("handle_call clientleave~n", []),
    p_network:update_network({delete, node()}),
    cast_to_peers({leave, node()}, Peers),
    {reply, ok, State#state{peers=[]}};

handle_call({leave, Peer}, _From, State=#state{peers=Peers}) ->
    ?DEBUG("handle_call leave~n", []),
    NewPeers = lists:delete(Peer, Peers),
    {reply, ok, State#state{peers=NewPeers}};

handle_call({newpeer, Peer}, _From, State=#state{peers=Peers}) ->
    ?DEBUG("handle_call newpeer~n", []),
    {reply, ok, State#state{peers=[Peer | Peers]}}.

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

send_messages() ->
    case io:get_line("Enter a message: ") of
        "--leave\n" ->
            ok;
        Message ->
            gen_server:call(?SERVER, {clientsend, Message}),
            send_messages()
    end.

