%%% By Haijun S, Jackson W, and Adnan J. 2024
%%% 
%%% Game Module
%%% 
-module(game).

-export([init/3, add_player/2, client_input/2, peer_input/3, client_leave/1,
         print_game_state/2]).

-record (game_state, {round = -1, prompts = [], peers = [], game_name,
         network}).

-define(ENDROUND, 2).
-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% init GameName Network Rounds
%% creates a gamestate with specified fields set

%% Create a new #game_state record
init(GameName, Network, Rounds) ->
    Prompts = generate_prompts:select_prompts("categories.txt", Rounds),
    #game_state{prompts= Prompts, game_name=GameName, network=Network}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SPECIFIC CLIENT INPUTS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% add_player JoiningPeer GameState
%% accept JoiningPeer into the game and alert other peers (if legal)

%% Adding players occurs during round -1
add_player(JoiningPeer, GameState=#game_state{round = -1, peers = Peers}) ->
    % Assumes round hasn't started
    update_peers({alert_new_peer, JoiningPeer}, GameState),
    GameState#game_state{peers = [JoiningPeer | Peers]};

%% Refuse adding a player if not in round -1
add_player(_JoiningPeer, _GameState) -> started.

%% client_leave GameState
%% processes leaving the game

client_leave(GameState) ->
    update_peers(leave, GameState),
    GameState.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GENERIC CLIENT INPUTS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% client_input Action GameState
%% processes an input from the client (typed input)
%% Action is the typed input

%% Round -1 accepts "ready" to mark as ready
client_input("ready", GameState=#game_state{peers=Peers, round=-1}) ->
    %% Uncomment to test join race condition
    %% timer:sleep(2000),
    update_peers(voteready, GameState),
    MePeer = gamepeer:get_me_peer(Peers),
    NewPeers = gamepeer:set_peer_data(ready, MePeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

%% Case for any input when at end of game (no prompts left)
client_input(_Input, GameState=#game_state{round=Round, prompts=[]})
when Round rem 2 == 0 ->
    io:format("Game is over, type --leave to leave the game!~n", []),
    GameState;

%% Even round = submission of answer
client_input(Answer, GameState=#game_state{peers=Peers, round=Round})
when Round rem 2 == 0 ->
    update_peers({submit, Answer}, GameState),
    MePeer = gamepeer:get_me_peer(Peers),
    NewPeers = gamepeer:set_peer_data(Answer, MePeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

%% Odd round = voting by username
client_input(Username, GameState=#game_state{peers=Peers, round=Round})
when Round rem 2 == 1 ->
    case gamepeer:check_valid_vote(Username, Peers) of true ->
        update_peers({vote, Username}, GameState),
        MePeer = gamepeer:get_me_peer(Peers),
        NewPeers = gamepeer:set_peer_data(Username, MePeer, Peers),
        %% TODO prevent voting twice
        NewPeers2 = gamepeer:add_peer_points(Username, NewPeers),
        NewGameState = GameState#game_state{peers=NewPeers2},
        advance_if_all_ready(NewGameState);
    _ ->
        io:format("Please enter a valid username, not including yourself!~n",
                  []),
        GameState
    end;

%% Base case
client_input(_Action, GameState) ->
    io:format("Unrecognized client action~n", []),
    GameState.

%%%%%%%%%%%%%%%%%%%%%%%%%
%% GENERIC PEER INPUTS %%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% peer_input Action FromPeer GameState
%% process an Action submitted by FromPeer, modify GameState

%% accept only voteready during round -1
peer_input(voteready, FromPeer, GameState=#game_state{peers=Peers, round=-1}) ->
    NewPeers = gamepeer:set_peer_data(ready, FromPeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    % print_game_state(check, NewGameState),
    advance_if_all_ready(NewGameState);

%% accept answer submissions during even rounds
peer_input({submit, Answer}, FromPeer,
           GameState=#game_state{peers=Peers, round=Round})
when Round rem 2 == 0 ->
    NewPeers = gamepeer:set_peer_data(Answer, FromPeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

%% accept username votes during odd rounds
peer_input({vote, Username}, FromPeer,
           GameState=#game_state{peers=Peers, round=Round})
when Round rem 2 == 1 ->
    NewPeers = gamepeer:set_peer_data(Username, FromPeer, Peers),
    NewPeers2 = gamepeer:add_peer_points(Username, NewPeers),
    NewGameState = GameState#game_state{peers=NewPeers2},
    advance_if_all_ready(NewGameState);

%% process a new peer joining the game on an unexpected round
%% restore ready votes, reset the round back to the voting round
peer_input({alert_new_peer, JoiningPeer}, _FromPeer,
           GameState=#game_state{peers=Peers, round=Round})
when Round =/= -1 ->
    NewPeers = gamepeer:unshift_data(Peers),
    NewGameState = GameState#game_state{peers=[JoiningPeer | NewPeers],
                                        round=-1},
    print_game_state(check, NewGameState),
    NewGameState;

%% process a new peer joining the game except this time it's supposed to happen
peer_input({alert_new_peer, JoiningPeer}, _FromPeer,
           GameState=#game_state{peers=Peers}) ->
    NewGameState = GameState#game_state{peers=[JoiningPeer | Peers]},
    print_game_state(check, NewGameState),
    NewGameState;

%% process a peer leaving the game (remove them from the peer list)
peer_input(leave, FromPeer, GameState=#game_state{peers=Peers}) ->
    NewPeers = gamepeer:remove_peer(FromPeer, Peers),
    NewGameState = GameState#game_state{peers=NewPeers},
    advance_if_all_ready(NewGameState);

%% Base case
peer_input(_Action, _FromPeer, GameState) ->
    io:format("Unrecognized peer action~n", []),
    GameState.


%%============================================================================%%
%% non-exported helpers
%%============================================================================%%

%% update_peers Action GameState
%% sends the Action to all peers in the specified format
%% includes sending my own peer so they know who it's from

%% beginning of the game when first user adds themselves
update_peers(_, #game_state{peers=[]}) -> ok;

update_peers(Action, #game_state{peers=Peers}) ->
    MePeer = gamepeer:get_me_peer(Peers),
    PeersSansMePeer = lists:delete(MePeer, Peers),
    PeerNodes = gamepeer:get_peer_nodes(PeersSansMePeer),
    lists:map(fun (PeerNode) ->
                  gen_server:cast({?SERVER, PeerNode},
                                  {peerinput, Action, MePeer})
              end, PeerNodes),
    ok.

%% print_game_state Precheck, GameState
%% prints the game state, but checks to make sure data has been submitted before
%% printing if Precheck is set to check. This allows the prompt to stay on the
%% screen and not wipe a user's input if oher people submit. This is the
%% compromise that we needed to make the TUI work without ncurses

%% perform the data check
print_game_state(check, GameState=#game_state{peers=Peers}) ->
    MePeer = gamepeer:get_me_peer(Peers),
    Data = gamepeer:get_peer_data(MePeer),
    case Data of
        not_ready -> ok;
        _ -> print_game_state(print, GameState)
    end;

%% just print
print_game_state(print, GameState=#game_state{round=Round, prompts=Prompts,
                                              game_name=GameName}) ->
    State = case Round of
                -1 -> "lobby";
                R1 when R1 rem 2 == 0 ->
                    case Prompts of 
                        [] -> "Game Over, type --leave to leave";
                        _ -> "Waiting on User input"
                    end;
                R2 when R2 rem 2 == 1 -> "Voting"
            end,
    io:format("~n~n~n~n~n~n~n~n~n~n~n~n", []),
    io:format("#####################################", []),
    io:format("#####################################~n", []),
    io:format("Game: ~s   State: ~s~n~n", [GameName, State]),
    print_round_prompt(GameState).

%% advance_if_all_ready GameState
%% move to the next round if the condition is met (everyone has submitted)

advance_if_all_ready(GameState=#game_state{peers=Peers, round=Round}) ->
    PeersData = gamepeer:get_data(Peers),
    %% accum ready state
    IsReady = lists:foldl(fun(Data, Accum) ->
                              case Data of
                                  not_ready -> not_ready;
                                  _ -> Accum
                              end
                          end, ready, PeersData),
    print_game_state(check, GameState),
    case IsReady of ready ->
        %% prep for next round & change round number
        NewPeers = gamepeer:shift_data(Peers),
        NewPeers2 = gamepeer:set_all_data(not_ready, NewPeers),
        NewGameState = GameState#game_state{peers=NewPeers2, round=Round + 1},
        %% print_game_state(NewGameState),
        NewGameState2 = shift_prompts(NewGameState),
        print_game_state(print, NewGameState2),
        NewGameState2;
    _ ->
        GameState
    end.

%% print_states GameState
%% print the leaderboard stats and prompt

%% end of game (no prompts)
print_stats(#game_state{prompts=[], peers=Peers}) ->
    % Show score here
    LeaderBoard = gamepeer:get_current_points(Peers),
    io:format("Final Leaderboard:~n", []),
    lists:map(fun ({Username, Points}) ->
                  io:format("~s: ~p~n", [Username, Points])
              end, LeaderBoard);

%% prompt remaining
print_stats(#game_state{prompts=[{Letter, Prompt} | _T], peers=Peers}) ->
    % Show score here
    LeaderBoard = gamepeer:get_current_points(Peers),
    io:format("Current Leaderboard:~n", []),
    lists:map(fun ({Username, Points}) ->
                  io:format("~s: ~p~n", [Username, Points])
              end, LeaderBoard),
    % Print prompts for next round
    io:format("~nPrompt: ~s, Letter: ~s~n~n", [Prompt, Letter]).

%% print_round_prompt GameState
%% print the information needed for each round

%% empty prompts = game over screen
print_round_prompt(#game_state{round=Round, prompts=[], peers=Peers})
when Round rem 2 == 0 ->
    io:format("Game Over!~n", []),
    % Show score here
    LeaderBoard = gamepeer:get_current_points(Peers),
    io:format("Leaderboard:~n", []),
    lists:map(fun ({Username, Points}) ->
                  io:format("~s: ~p~n", [Username, Points])
              end, LeaderBoard);

%% even round = enter answer or answer submitted
print_round_prompt(GameState=#game_state{round=Round, peers=Peers})
when Round rem 2 == 0 ->
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

%% odd round = vote by username or voted username
print_round_prompt(GameState=#game_state{round=Round, peers=Peers})
when Round rem 2 == 1 ->
    print_stats(GameState),
    io:format("Responses:~n", []),
    UsernameDataOld = gamepeer:get_username_data_old(Peers),
    lists:map(fun ({Username, Data}) ->
                  io:format("~s: ~s~n", [Username, Data])
              end, UsernameDataOld),
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

%% lobby prompt (vote ready or voted ready)
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

%% shift_prompts GameState
%% pop the prompts to advance to the next prompt if necessary

%% game over (do nothing)
shift_prompts(GameState=#game_state{round=Round, prompts=[]})
when Round rem 2 == 0 ->
    % On leaving game, we also have to kill the input
    GameState;

%% even round (do nothing)
shift_prompts(GameState=#game_state{round=Round}) when Round rem 2 == 0 ->
    GameState;

%% odd round (pop)
shift_prompts(GameState=#game_state{round=Round, prompts=[_P | Prompts]})
when Round rem 2 == 1 ->
    GameState#game_state{prompts=Prompts}.
