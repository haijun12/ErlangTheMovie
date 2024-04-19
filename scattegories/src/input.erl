-module(input).

-export([parse/1, handle_input/0]).

-define(DEBUG(Format, Args), void).

parse("start") ->
    votestart;

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
