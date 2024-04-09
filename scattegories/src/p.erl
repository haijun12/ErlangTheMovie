-module(p).
-behaviour(gen_server).

-export([create/1, join/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name, peers, inRoom}).

-define (COOKIE, scattegories).
-define (SERVER, scattegories).

%% -define(DEBUG(Format, Args), io:format(Format, Args)).
-define(DEBUG(Format, Args), void).

setup(Name) ->
    erlang:set_cookie(?COOKIE),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Name], []),
    ok.

setup2() ->
    send_messages(),
    gen_server:cast(?SERVER, {clientleave}),
    ok.

create(Name) ->
    setup(Name),
    setup2().

join(Name, Node) ->
    setup(Name),
    gen_server:cast(?SERVER, {clientjoin, Node}),
    setup2().

init([Name]) ->
    {ok, #state{name=Name, peers=[], inRoom = false}}.

handle_call({join, Peer}, _From, State=#state{peers=Peers}) ->
    ?DEBUG("handle_call join~n", []),
    cast_to_peers({newpeer, Peer}, Peers),
    {reply, {ok, Peers}, State#state{peers=[Peer | Peers]}}.

handle_cast({clientsend, Message}, State=#state{name=Name, peers=Peers}) ->
    ?DEBUG("handle_cast clientsend~n", []),
    cast_to_peers({message, Message, Name}, Peers),
    {noreply, State};

handle_cast({message, Message, Fromname}, State) ->
    ?DEBUG("handle_cast message~n", []),
    io:format("~s: ~s", [Fromname, Message]),
    {noreply, State};

handle_cast({clientjoin, Peer}, State) ->
    ?DEBUG("handle_cast clientjoin~n", []),
    {ok, NewPeers} = gen_server:call({?SERVER, Peer}, {join, node()}),
    {noreply, State#state{peers=[Peer | NewPeers]}};

handle_cast({clientleave}, State=#state{peers=Peers}) ->
    ?DEBUG("handle_cast clientleave~n", []),
    cast_to_peers({leave, node()}, Peers),
    {noreply, State#state{peers=[]}};

handle_cast({leave, Peer}, State=#state{peers=Peers}) ->
    ?DEBUG("handle_cast leave~n", []),
    NewPeers = remove_peer(Peer, Peers),
    {noreply, State#state{peers=NewPeers}};

handle_cast({newpeer, Peer}, State=#state{peers=Peers}) ->
    ?DEBUG("handle_cast newpeer~n", []),
    {noreply, State#state{peers=[Peer | Peers]}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cast_to_peers(Message, [Peer | T]) ->
    gen_server:cast({?SERVER, Peer}, Message),
    cast_to_peers(Message, T);

cast_to_peers(_, []) ->
    ok.

remove_peer(Peer, [Peer | T]) -> T;
remove_peer(Peer, [Peer2 | T]) -> [Peer2 | remove_peer(Peer, T)];
remove_peer(_Peer, []) -> []. %% might be an error


send_messages() ->
    case io:get_line("Enter a message: ") of
        "--leave\n" ->
            ok;
        Message ->
            gen_server:cast(?SERVER, {clientsend, Message}),
            send_messages()
    end.
