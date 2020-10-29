-module(rebar3_cuttlefish_tar).
-behaviour(provider).

-export([init/1
        ,do/1
        ,format_error/1]).

-define(PROVIDER, tar).
-define(NAMESPACE, default).
-define(DEPS, [{?NAMESPACE, release}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, ?NAMESPACE},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 cuttlefish tar"}, % How to use the plugin
            {opts, rebar3_cuttlefish_release:supported_options()},                   % list of options understood by the plugin
            {short_desc, "Rebar3 cuttlefish release tarball plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_relx:do(rlx_prv_release, "tar", ?PROVIDER, State).

-spec format_error(any()) ->  iolist().
format_error(Error) ->
    io_lib:format("~p", [Error]).
