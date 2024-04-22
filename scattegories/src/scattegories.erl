-module(scattegories).

-export([start/0]).
% Im thinking interpreter stuff here
start() ->
    UserName = io:get_line("Enter a username: "),
    [_T | Name] = lists:reverse(UserName),
    RealUserName = lists:reverse(Name),
    start_game(RealUserName).
    

start_game(UserName) ->
    Mode = io:get_line("Would you like to create or join a room? [Enter: create or join]:  "),
    case Mode of "create\n" ->
        io:format("Creating a game...~n", []),
        Room = io:get_line("Enter a RoomName:  "),
        [_H | Room1] = lists:reverse(Room),
        RoomName = lists:reverse(Room1),
        p:create(UserName, RoomName, nothing),
        start_game(UserName);
    "join\n" ->
        Peer = io:get_line("Enter a Node you would like to join:  "),
        [_H | Peer1] = lists:reverse(Peer),
        Peer2 = lists:reverse(Peer1),
        PeerNode = list_to_atom(Peer2),
        p:join(UserName, PeerNode, nothing),
        start_game(UserName);
    _ ->
        io:format("Invalid input. Please enter 'c' to create a room or 'j' to join a room.~n"),
            start()
    end.


