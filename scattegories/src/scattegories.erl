%%% By Haijun S, Jackson W, and Adnan J. 2024
%%% 
%%% Scattegories Module
%%% 
-module(scattegories).

-export([start/1]).

% start(UserName): starts the interpreter for the game
start(UserName) ->
    Mode = io:get_line("Would you like to create or join a room? " ++
                       "[Enter: create or join]:  "),
    case Mode of "create\n" ->
        io:format("Creating a game...~n", []),
        Room = io:get_line("Enter a RoomName:  "),
        Rounds = io:get_line("Enter the number of rounds you would like to " ++
                             "play: "),
        [_H | Room1] = lists:reverse(Room),
        {Rounds1, _} = string:to_integer(Rounds),
        RoomName = lists:reverse(Room1),
        p:create(UserName, RoomName, nothing, Rounds1),
        start(UserName);
    "join\n" ->
        Peer = io:get_line("Enter a Node you would like to join:  "),
        [_H | Peer1] = lists:reverse(Peer),
        Peer2 = lists:reverse(Peer1),
        PeerNode = list_to_atom(Peer2),
        p:join(UserName, PeerNode, nothing),
        start(UserName);
    _ ->
        io:format("Invalid input. Please enter 'create' to create a room or " ++
                  "'join' to join a room.~n"),
            start(UserName)
    end.


