-module(game).

-export([init/2, add_player/2, client_input/2, peer_input/3, client_leave/1, print_game_state/2]).

-record (game_state, {round = -1, prompts = [], peers = [], game_name, network}).

-define(ENDROUND, 2).
-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).

%% -define(DEBUG(Format, Args), io:format("[DEBUG] [game.erl] " ++ Format, Args)).
-define(DEBUG(Format, Args), void).

init(GameName, Network) ->
    Prompts = generate_prompts:select_prompts("categories.txt", ?ENDROUND),
    #game_state{prompts= Prompts, game_name=GameName, network=Network}.

add_player(JoiningPeer, GameState=#game_state{round = -1, peers = Peers}) ->
    ?DEBUG("Adding a new player ~n", []),
    ?DEBUG("New peer: ~p~n", [JoiningPeer]),
    ?DEBUG("Peer list: ~p~n", [Peers]),
    % Assumes round hasn't started
    update_peers({alert_new_peer, JoiningPeer}, GameState),
    GameState#game_state{peers = [JoiningPeer | Peers]};

add_player(_JoiningPeer, _GameState) -> started.

client_leave(GameState) ->
    update_peers(leave, GameState),
    GameState.

client_input("ready", GameState=#game_state{peers=Peers, round=-1}) ->
    %% timer:sleep(1000),
    update_peers(voteready, GameState),
    MePeer = gamepeer:get_me_peer(Peers),
    NewPeers = gamepeer:set_peer_data(ready, MePeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

client_input(_Input, GameState=#game_state{round=Round, prompts=[]}) when Round rem 2 == 0 ->
    io:format("Game is over, type leave to leave the game!~n", []),
    GameState;

client_input(Input, GameState=#game_state{peers=Peers, round=Round}) when Round rem 2 == 0 ->
    update_peers({submit, Input}, GameState),
    MePeer = gamepeer:get_me_peer(Peers),
    NewPeers = gamepeer:set_peer_data(Input, MePeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

client_input(Input, GameState=#game_state{peers=Peers, round=Round}) when Round rem 2 == 1 ->
    case gamepeer:check_valid_vote(Input, Peers) of true ->
        update_peers({vote, Input}, GameState),
        MePeer = gamepeer:get_me_peer(Peers),
        NewPeers = gamepeer:set_peer_data(Input, MePeer, Peers),
        %% TODO can't vote twice
        NewPeers2 = gamepeer:add_peer_points(Input, NewPeers),
        NewGameState = GameState#game_state{peers=NewPeers2},
        advance_if_all_ready(NewGameState);
    _ ->
        io:format("You cannot vote for yourself!~n", []),
        GameState
    end;

client_input(Action, GameState) ->
    io:format("Unrecognized client action~n", []),
    ?DEBUG("Action was ~p~n", [Action]),
    GameState.

peer_input(voteready, FromPeer, GameState=#game_state{peers=Peers, round=-1}) ->
    NewPeers = gamepeer:set_peer_data(ready, FromPeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

peer_input({submit, Input}, FromPeer, GameState=#game_state{peers=Peers, round=Round}) when Round rem 2 == 0 ->
    NewPeers = gamepeer:set_peer_data(Input, FromPeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},

    advance_if_all_ready(NewGameState);

peer_input({vote, Input}, FromPeer, GameState=#game_state{peers=Peers, round=Round}) when Round rem 2 == 1 ->
    NewPeers = gamepeer:set_peer_data(Input, FromPeer, Peers),
    % Update peers points
    NewPeers2 = gamepeer:add_peer_points(Input, NewPeers),
    NewGameState = GameState#game_state{peers=NewPeers2},
    advance_if_all_ready(NewGameState);

peer_input({alert_new_peer, JoiningPeer}, _FromPeer, GameState=#game_state{peers=Peers, round=Round}) when Round =/= -1 ->
    NewPeers = gamepeer:unshift_data(Peers),
    GameState#game_state{peers=[JoiningPeer | NewPeers], round=-1};

peer_input({alert_new_peer, JoiningPeer}, _FromPeer, GameState=#game_state{peers=Peers}) ->
    %% print_game_state(GameState),
    GameState#game_state{peers=[JoiningPeer | Peers]};

peer_input(leave, FromPeer, GameState=#game_state{peers=Peers}) ->
    NewPeers = gamepeer:remove_peer(FromPeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

peer_input(Action, _FromPeer, GameState) ->
    io:format("Unrecognized peer action~n", []),
    ?DEBUG("Action was ~p~n", [Action]),
    ?DEBUG("FromPeer was ~p~n", [FromPeer]),
    GameState.


%%============================================================================%%
%% non-exported helpers
%%============================================================================%%

update_peers(_, #game_state{peers=[]}) ->
    %% this will only happen at the beginning of the game when the first user
    %% adds themselves
    [ok];
update_peers(Action, #game_state{peers=Peers}) ->
    MePeer = gamepeer:get_me_peer(Peers),
    PeersSansMePeer = lists:delete(MePeer, Peers),
    PeerNodes = gamepeer:get_peer_nodes(PeersSansMePeer),
    util:pmap(fun (PeerNode) -> gen_server:call({?SERVER, PeerNode}, {peerinput, Action, MePeer}) end, PeerNodes).

print_game_state(check, GameState=#game_state{peers=Peers}) ->
    MePeer = gamepeer:get_me_peer(Peers),
    Data = gamepeer:get_peer_data(MePeer),
    case Data of
        not_ready -> ok;
        _ -> print_game_state(print, GameState)
    end;

print_game_state(print, GameState=#game_state{round=Round, prompts=Prompts, game_name=GameName}) ->
    State = case Round of
                -1 -> "lobby";
                R1 when R1 rem 2 == 0 -> case Prompts of 
                                            [] -> "Game Over, type --leave to leave";
                                            _ -> "Inputting"
                                            end;
                R2 when R2 rem 2 == 1 -> "Voting"
            end,
    % State2 = case Prompts of
    %              [] -> "game over";
    %              _ -> State
    %          end,
    io:format("~n~n~n~n~n~n~n~n~n~n~n~n", []),
    io:format("##########################################################################~n", []),
    io:format("Game: ~s   State: ~s~n~n", [GameName, State]),
    print_round_prompt(GameState).

advance_if_all_ready(GameState=#game_state{peers=Peers, round=Round}) ->
    PeersData = gamepeer:get_data(Peers),
    IsReady = lists:foldl(fun(Data, Accum) ->
                              case Data of
                                  not_ready -> not_ready;
                                  _ -> Accum
                              end
                          end, ready, PeersData),
    case IsReady of ready ->
        NewPeers = gamepeer:shift_data(Peers),
        NewPeers2 = gamepeer:set_all_data(not_ready, NewPeers),
        NewGameState = GameState#game_state{peers=NewPeers2, round=Round + 1},
        %% print_game_state(NewGameState),
        shift_prompts(NewGameState);
    _ ->
        GameState
    end.

print_stats(#game_state{prompts=[], peers=Peers}) ->
    % Show score here
    LeaderBoard = gamepeer:get_current_points(Peers),
    io:format("Final Leaderboard:~n", []),
    lists:map(fun ({Username, Points}) -> io:format("~s: ~p~n", [Username, Points]) end, LeaderBoard);

print_stats(#game_state{prompts=[{Letter, Prompt} | _T], peers=Peers}) ->
    % Show score here
    LeaderBoard = gamepeer:get_current_points(Peers),
    io:format("Current Leaderboard:~n", []),
    lists:map(fun ({Username, Points}) -> io:format("~s: ~p~n", [Username, Points]) end, LeaderBoard),
    % Print prompts for next round
    io:format("~nPrompt: ~s, Letter: ~s~n~n", [Prompt, Letter]).

print_round_prompt(#game_state{round=Round, prompts=[], peers=Peers}) when Round rem 2 == 0 ->
    io:format("Game Over!~n", []),
    % Show score here
    LeaderBoard = gamepeer:get_current_points(Peers),
    io:format("Leaderboard:~n", []),
    lists:map(fun ({Username, Points}) -> io:format("~s: ~p~n", [Username, Points]) end, LeaderBoard);

print_round_prompt(GameState=#game_state{round=Round, peers=Peers}) when Round rem 2 == 0 ->
    print_stats(GameState),
    io:format("Submitted: ~n~n", []),
    UsernameData = gamepeer:get_username_data(Peers),
    lists:map(fun ({Username, Data}) ->
                  Data2 = case Data of
                      not_ready -> "";
                      _ -> ": SUBMITTED"
                  end,
                  io:format("~s~s~n", [Username, Data2])
              end, UsernameData),
    MePeer = gamepeer:get_me_peer(Peers),
    case gamepeer:get_peer_data(MePeer) of
        not_ready ->
            io:format("Enter your answer:~n");
        Data ->
            io:format("Answered: ~s~n", [Data])
    end;

print_round_prompt(GameState=#game_state{round=Round, peers=Peers}) when Round rem 2 == 1 ->
    print_stats(GameState),
    io:format("Responses:~n", []),
    UsernameDataOld = gamepeer:get_username_data_old(Peers),
    lists:map(fun ({Username, Data}) -> io:format("~s: ~s~n", [Username, Data]) end, UsernameDataOld),
    io:format("~nVotes: ~n", []),
    UsernameData = gamepeer:get_username_data(Peers),
    lists:map(fun ({Username, Data}) ->
                  Data2 = case Data of
                      not_ready -> "";
                      _ -> ": VOTED"
                  end,
                  io:format("~s~s~n", [Username, Data2])
              end, UsernameData),
    MePeer = gamepeer:get_me_peer(Peers),
    case gamepeer:get_peer_data(MePeer) of
        not_ready ->
            io:format("Vote by username (not own username!):~n");
        Data ->
            io:format("Voted: ~s~n", [Data])
    end;


print_round_prompt(#game_state{round=-1, peers=Peers}) ->
    io:format("Ready: ~n", []),
    UsernameData = gamepeer:get_username_data(Peers),
    lists:map(fun ({Username, Data}) ->
                  Data2 = case Data of
                      not_ready -> "";
                      _ -> ": READY"
                  end,
                  io:format("~s~s~n", [Username, Data2])
              end, UsernameData),
    MePeer = gamepeer:get_me_peer(Peers),
    case gamepeer:get_peer_data(MePeer) of
        not_ready ->
            io:format("Type ready when ready:~n");
        _ ->
            io:format("Ready to play~n", [])
    end.

% h@Mac-PWH2RVFGJ2

shift_prompts(GameState=#game_state{round=Round, prompts=[]}) when Round rem 2 == 0 ->
    % On leaving game, we also have to kill the input
    io:format("hello~n~n~n~n", []),
    GameState;

shift_prompts(GameState=#game_state{round=Round}) when Round rem 2 == 0 ->
    GameState;


shift_prompts(GameState=#game_state{round=Round, prompts=[_P | Prompts]}) when Round rem 2 == 1 ->
    GameState#game_state{prompts=Prompts}.
