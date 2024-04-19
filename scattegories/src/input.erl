-module(input).

-export([parse/1, handle_input/0]).

-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).

parse("start\n") ->
    voteready;

parse(_) ->
    unrecognized.


handle_input() ->
    case io:get_line("SCATTERGORIES # ") of
        "leave\n" ->
            ok;
        "list\n" ->
            gen_server:call(?SERVER, {list}),
            handle_input();
        Input ->
            Action = parse(Input),
            gen_server:call(?SERVER, {clientinput, Action}),
            handle_input()
    end.
