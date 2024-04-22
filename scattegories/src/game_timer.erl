-module(game_timer).
-export([start_timer/1]).

start_timer(Duration) ->
    timer(Duration).

%timer(Duration) ->%
%   timer(Duration, Duration).%

timer(0) ->
    io:fwrite("\rTime's up!               ");

timer(Remaining) ->
    io:fwrite("\r Time remaining: ~2B seconds              ", [Remaining]),
    timer:sleep(1000),
    timer(Remaining - 1).