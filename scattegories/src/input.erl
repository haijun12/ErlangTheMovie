-module(input).

-export([handle_input/0]).

-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).


handle_input() ->
    case io:get_line("SCATTERGORIES # ") of
        "--leave\n" ->
            ok;
        Input ->
            [_H | Input2] = lists:reverse(Input),
            Input3 = lists:reverse(Input2),
            gen_server:cast(?SERVER, {clientinput, Input3}),
            handle_input()
    end.
