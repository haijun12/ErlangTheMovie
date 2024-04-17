-module(p_network).
-behaviour(gen_server).

-export ([start/0, update_network/1]).
-export ([init/1, handle_call/3, handle_cast/2, terminate/2]).

-define(COOKIE, scattegories).
-define(SERVER, peerdistribution).

-define(DEBUG(Format, Args), io:format("[DEBUG] " ++ Format, Args)).
%% -define(DEBUG(Format, Args), void).

-record(state, {map}).

start() ->
    erlang:set_cookie(?COOKIE),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_network(Message) -> 
    ?DEBUG("update_network of ~p~n", [Message]),
    case Message of
        {add, Peer, GameName} -> gen_server:cast(?SERVER, {add, Peer, GameName});
        {delete, Peer} -> gen_server:cast(?SERVER, {delete, Peer});
        list -> gen_server:cast(?SERVER, {list})
    end.

init([e]) -> 
    {ok, #state{map=[]}}.


handle_call({list}, _From, State = #state{map=Map}) ->  
    ?DEBUG("handle_call list~n", []),
    {reply, Map, State}.

handle_cast({list}, State = #state{map=Map}) ->  
    ?DEBUG("handle_cast list~p~n", [Map]),
    {noreply, State};

handle_cast({add, Peer, GameName}, State = #state{map=Map}) ->
    ?DEBUG("handle_call add peer~n", []),
    {noreply, State#state{map=[ {Peer, GameName} | Map]}};

handle_cast({delete, Peer}, State = #state{map=Map}) ->
    ?DEBUG("handle_call delete peer~n", []),
    NewMap = lists:filter(fun ({CurrPeer, _}) -> Peer =/= CurrPeer end, Map),
    {noreply, State#state{map=NewMap}}.

terminate(_Reason, _State) ->
    ok.

