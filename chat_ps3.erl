%%%-------------------------------------------------------------------
%%% @author Mark A. Sheldon <msheldon@cs.tufts.edu>
%%% @copyright (C) 2020, Mark A. Sheldon
%%% @doc
%%%
%%% @end
%%% Created : 23 Mar 2020 by Mark A. Sheldon <msheldon@cs.tufts.edu>
%%%-------------------------------------------------------------------
-module(chat).

-behaviour(gen_server).

%% Server API
-export([start_link/0, stop/0]).

%% Client API
-export([join_room/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

%% -define(DEBUG(Format, Args), io:format(Format, Args)).
-define(DEBUG(Format, Args), void).

%% -record(state, {}).

%%%===================================================================
%%% Server API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    ?DEBUG("~s ~s~n", [?SERVER, node()]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%%%===================================================================
%%% Client API
%%%===================================================================


% join_room
% joins a client to a room
% spawns a process to receive and print messages
% prompts the user to input messages which are sent to the server

join_room(Node, Room, User) ->
    Server = {?SERVER, Node},
    Client = self(),
    Listener = spawn(fun () -> c_receive_messages(Server, Room, Client) end),
    gen_server:cast(Server, {subscribe, User, Room, Listener}),
    RES = c_send_messages(string:concat(User, ": "), Room, Server),
    gen_server:cast(Server, {unsubscribe, User, Room}),
    Listener ! {Client, done},
    RES.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.

% initialize to an empty list of rooms
init([]) ->
    process_flag(trap_exit, true),
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
    {reply, Reply :: term(), NewState :: term(), hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), Timeout :: timeout()} |
    {noreply, NewState :: term(), hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

% list - return everyone in a room
handle_call({list, Room}, _From, State) ->
    ?DEBUG("list ~n", []),
    {value, {_, Users}} = lists:search(fun({RN, _}) -> RN == Room end, State),
    Reply = lists:map(fun({Name, _}) -> Name end, Users),
    {reply, Reply, State};

% generic case
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.

% post - when a user sends a message
handle_cast({post, Room, Message}, State) ->
    ?DEBUG("S POST~n", []),
    s_sendtoroom(Message, Room, State),
    {noreply, State};

% subscribe - when a user subscribes to a room
handle_cast({subscribe, User, Room, Listener}, State) ->
    ?DEBUG("S SUBSCRIBE~n", []),
    NewState = s_findadd(User, Room, Listener, State),
    {noreply, NewState};

% unsubscribe - when a user unsubscribes to a room
handle_cast({unsubscribe, User, Room}, State) ->
    ?DEBUG("S UNSUBSCRIBE~n", []),
    NewState = s_findremove(User, Room, State),
    {noreply, NewState};

% generic case
handle_cast(_Request, State) ->
    ?DEBUG("S UNKNOWN CAST~n", []),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% This sends a stop message to all connected users
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().

terminate(Reason, State) ->
    lists:foreach(fun ({Room, Users}) ->
       lists:foreach(fun ({_User, Listener}) ->
            Listener ! {node(), Room, stop, Reason}
        end, Users)
    end, State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Client helper functions
%%%===================================================================


%%% c_receive_messages
%%% handle receiving messages

c_receive_messages({Serv, Node}, Room, Client) ->
    ?DEBUG("CRM ROOM ~s~n", [Room]),
    receive
        {Node, Room, message, Message} ->
            io:format(Message),
            c_receive_messages({Serv, Node}, Room, Client);
        {Node, Room, stop, Reason} ->
            io:format("Server terminated with reason: ~s~n", [Reason]),
            io:format("Please type '--quit' to exit~n"),
            ok;
        {Client, done} ->
            ok;
        Any ->
            io:format("Unhandled message: ~p~n", [Any]),
            c_receive_messages({Serv, Node}, Room, Client)
    end.


%%% c_send_messages
%%% handle sending messages to the server
%%% as well as special commands

c_send_messages(Prefix, Room, Server) ->
    case io:get_line("Enter a message: ") of
        "--quit\n" ->
            ok;
        "--list\n" ->
            People = gen_server:call(Server, {list, Room}),
            c_printusers(People),
            c_send_messages(Prefix, Room, Server);
        Message ->
            gen_server:cast(Server,
                           {post, Room, string:concat(Prefix, Message)}),
            c_send_messages(Prefix, Room, Server)
    end.


%%% c_printusers
%%% pretty print a list of users

c_printusers([]) ->
    io:format("No Users"),
    ok;
c_printusers([H | T]) ->
    io:format("Users: [~s", [H]),
    c_printusers(T, in).
c_printusers([], in) ->
    io:format("]~n"),
    ok;
c_printusers([H | T], in) ->
    io:format(", ~s", [H]),
    c_printusers(T, in).

%%%===================================================================
%%% Server helper functions
%%%===================================================================


%%% s_findadd
%%% Add user to room, or create room with user if room doesn't exist

s_findadd(User, Room, Listener, []) ->
    [{Room, [{User, Listener}]}];
s_findadd(User, Room, Listener, [{Room, Users} | T]) ->
    [{Room, [{User, Listener} | Users]} | T];
s_findadd(User, Room, Listener, [H | T]) ->
    [H | s_findadd(User, Room, Listener, T)].


%%% s_findremove
%%% Remove user from a room

s_findremove(_User, _Room, []) -> [];
s_findremove(User, Room, [{Room, Users} | T]) ->
    [{Room, s_findremoveuser(User, Users)} | T];
s_findremove(User, Room, [H | T]) ->
    [H | s_findremove(User, Room, T)].

s_findremoveuser(_User, []) -> [];
s_findremoveuser(User, [{User, _} | T]) -> T;
s_findremoveuser(User, [H | T]) ->
    [H | s_findremoveuser(User, T)].


%%% s_sendtoroom
%%% send a message to all users in a specific room

s_sendtoroom(_Message, _Room, []) -> error;
s_sendtoroom(Message, Room, [{Room, Users} | _T]) ->
    lists:foreach(fun ({_User, Listener}) -> 
        Listener ! {node(), Room, message, Message}
    end, Users);
s_sendtoroom(Message, Room, [_Other | T]) ->
    s_sendtoroom(Message, Room, T).
