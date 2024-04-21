-module(game_timer).
-export([start_timer/1]).

start_timer(Duration) ->
    spawn(fun() -> timer(Duration) end).

timer(Duration) ->
    timer(Duration, Duration).

timer(0, _) ->
    io:fwrite("\rTime's up!~n");

timer(Remaining, Duration) ->
    io:fwrite("\rTime remaining: ~2B seconds", [Remaining]),
    timer:sleep(1000),
    timer(Remaining - 1, Duration).