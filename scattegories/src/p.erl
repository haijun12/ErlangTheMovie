%%% By Haijun S, Jackson W, and Adnan J. 2024
%%% 
%%% P Module
%%% 
-module(p).
-behaviour(gen_server).

-export([create/4, join/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).

%% -define(DEBUG(Format, Args), io:format("[DEBUG] [p.erl] " ++ Format, Args)).
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
    input:handle_input(),
    gen_server:cast(?SERVER, {clientleave}),
    ok.

create(MePeerName, GameName, Network, Rounds) ->
    io:format("~p~n", [Rounds]),
    GameState = game:init(GameName, Network, Rounds),
    MePeerNode = node(),
    MePeer = gamepeer:init(MePeerNode, MePeerName),
    GameState2 = game:add_player(MePeer, GameState),
    setup(GameState2),
    game:print_game_state(print, GameState2),
    setup2().

join(MePeerName, JoinPeerNode, Network) ->
    MePeerNode = node(),
    MePeer = gamepeer:init(MePeerNode, MePeerName),
    setup({joining, MePeer, Network}),
    case gen_server:cast(?SERVER, {clientjoin, JoinPeerNode}) of
        ok -> setup2();
        started ->
            io:format("The lobby has already started~n", [])
    end.


%%============================================================================%%
%%============================================================================%%
%%============================================================================%%
%% gen_server
%%============================================================================%%
%%============================================================================%%
%%============================================================================%%

init([GameState]) ->
    {ok, GameState}.

%% We don't need the from field
handle_call(Request, _From, State) ->
    handle_call(Request, State).

%%============================================================================%%
%% joining
%%============================================================================%%

handle_call({join, JoiningPeer}, GameState) ->
    ?DEBUG("handle_call join~n", []),
    case game:add_player(JoiningPeer, GameState) of
        started -> {reply, started, GameState};
        NewGameState ->
            % game:print_game_state(print, NewGameState),
            {reply, {ok, NewGameState}, NewGameState}
    end.

handle_cast({clientjoin, JoinPeerNode}, {joining, MePeer, Network}) ->
    ?DEBUG("handle_call clientjoin~n", []),
    case gen_server:call({?SERVER, JoinPeerNode}, {join, MePeer}) of
        {ok, GameState} -> 
            game:print_game_state(print, GameState),
            {noreply, GameState};
        started -> {stop, normal, {joining, MePeer, Network}}
    end;

%%============================================================================%%
%% client input
%%============================================================================%%

handle_cast({clientinput, Action}, GameState) ->
    NewGameState = game:client_input(Action, GameState),
    {noreply, NewGameState};

%%============================================================================%%
%% peer input
%%============================================================================%%

handle_cast({peerinput, Action, FromPeer}, GameState) ->
    NewGameState = game:peer_input(Action, FromPeer, GameState),
    {noreply, NewGameState};

%%============================================================================%%
%% leave
%%============================================================================%%

handle_cast({clientleave}, GameState) ->
    game:client_leave(GameState),
    {stop, normal, GameState};

%% handle_call({peerinput, FromPeer, leave}, GameState) ->
    %% ?DEBUG("handle_call leave~n", []),
    %% NewGameState = game:peer_leave(FromPeer, GameState),
    %% {reply, ok, NewGameState}.

%%============================================================================%%
%% list
%%============================================================================%%

%% handle_call({list}, State=#state{network=NetworkName}) ->
    %% ?DEBUG("handle_call list~n", []),
    %% PeerMap = gen_server:call({?DSERVER, NetworkName}, {list}),
    %% io:format("~p~n", [PeerMap]),
    %% {reply, ok, State}.

%%============================================================================%%
%% 
%%============================================================================%%

handle_cast(_Request, State) ->
    {noreply, State}.

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


