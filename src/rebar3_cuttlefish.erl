-module(rebar3_cuttlefish).

-export([init/1]).

init(State) ->
    {ok, State1} = rebar3_cuttlefish_release:init(State),
    rebar3_cuttlefish_tar:init(State1).
