-module(p).
-behaviour(gen_server).

-export([start_link/1, send_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define (COOKIE, scattegories).

-record(state, {name, peers}).

start_link(Name) ->
    erlang:set_cookie(?COOKIE),
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

send_message({Peer, Node}, Message) ->
    gen_server:cast({Peer, Node}, {message, Message}).

init([Name]) ->
    {ok, #state{name=Name, peers=[]}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({message, Message}, State=#state{name=Name}) ->
    io:format("~p received message: ~p~n", [Name, Message]),
    {noreply, State};

handle_cast({connect, Peer}, State=#state{peers=Peers}) ->
    {noreply, State#state{peers=[Peer|Peers]}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.