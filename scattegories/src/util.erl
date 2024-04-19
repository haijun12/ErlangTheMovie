-module(util).
-export([pmap/2, pm_runfun/3]).

-define(DEBUG(Format, Args), io:format("[DEBUG] [game.erl] " ++ Format, Args)).
%% -define(DEBUG(Format, Args), void).

pmap(F, L) ->
    Pidlist = pm_pidlist(F, L),
    pm_getresults(Pidlist).

pm_pidlist(_, []) -> [];
pm_pidlist(F, [H | T]) ->
    Self = self(),
    [spawn(util, pm_runfun, [Self, F, H]) | pm_pidlist(F, T)].

pm_runfun(Parent, F, V) ->
    Self = self(),
    Parent ! {Self, F(V)}.

pm_getresults([]) -> [];
pm_getresults([Pid | T]) ->
    receive
        {Pid, MV} -> 
            [MV | pm_getresults(T)]
    end.

