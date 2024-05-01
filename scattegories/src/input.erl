%%% By Haijun S, Jackson W, and Adnan J. 2024
%%% 
%%% Input Module
%%% 
-module(input).

-export([handle_input/0]).

-define (COOKIE, scattegories).
-define (SERVER, scattegories).
-define (DSERVER, peerdistribution).

%% handle_input
%% input loop, send via a cast to client_input in p.erl

handle_input() ->
    case io:get_line("SCATTERGORIES # ") of
        "--leave\n" ->
            ok;
        Input ->
            % Gets rid of newline
            [_H | Input2] = lists:reverse(Input),
            Input3 = lists:reverse(Input2),
            gen_server:cast(?SERVER, {clientinput, Input3}),
            handle_input()
    end.
