-module(scattegories).

-export([start/0]).
% Im thinking interpreter stuff here
start() ->
    UserName = io:get_line("Enter a username"),
    Mode = io:get_line("Would you like to create or join a room? [Enter: c or j]"),
    case Mode of "c" ->
        RoomName = io:get_line("Creating a game... Enter a RoomName:"),
        p:create(UserName, RoomName, nothing);
    "j" ->
        PeerNode = io:get_line("Enter a Node you would like to join"),
        p:join(UserName, PeerNode, nothing);
    _ ->
        io:format("Invalid input. Please enter 'c' to create a room or 'j' to join a room.~n"),
            start()
    end.


