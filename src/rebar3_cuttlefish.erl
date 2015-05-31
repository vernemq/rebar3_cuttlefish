-module(rebar3_cuttlefish).
-behaviour(provider).

-export([init/1
        ,do/1
        ,format_error/1]).

-include_lib("providers/include/providers.hrl").

-define(PROVIDER, cuttlefish).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 cuttlefish"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Rebar3 cuttlefish plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running cuttlefish schema generator", []),
    Relx = rebar_state:get(State, relx, []),
    ReleaseDir = filename:join(rebar_dir:base_dir(State), "rel"),

    {release, {Name, _Vsn}, _} = lists:keyfind(release, 1, Relx),
    TargetDir = filename:join([ReleaseDir, Name]),
    case lists:keyfind(overlay, 1, Relx) of
        {overlay, Overlays} when is_list(Overlays) ->
            Schemas = schemas(Overlays, TargetDir),

            case cuttlefish_schema:files(Schemas) of
                {errorlist, _Es} ->
                    %% These errors were already printed
                    error;
                {_Translations, Mappings, _Validators} ->
                    make_default_file(State, Name, TargetDir, Mappings)
            end,
            {ok, State};
        false ->
            {ok, State};
        _ ->
            ?PRV_ERROR({bad_overlay_entry})

    end.

-spec format_error(any()) ->  iolist().
format_error({bad_overlay_entry}) ->
    io_lib:format("{overlay, [...]} entry in relx config must be a list.~n", []).

make_default_file(State, Name, TargetDir, Mappings) ->
    File = rebar_state:get(State, cuttlefish_filename, io_lib:format("~s.conf", [Name])),
    Filename = filename:join([TargetDir, "etc", File]),
    cuttlefish_conf:generate_file(Mappings, Filename).

schemas(Overlays, TargetDir) ->
    SchemaOverlays =
        lists:filter(fun(Overlay) ->
                         element(1, Overlay) =:= template
                             andalso filename:extension(element(3, Overlay)) =:= ".schema"
                     end, Overlays),

    lists:sort([lists:flatten(filename:join(TargetDir, element(3, Schema)))
               || Schema <- SchemaOverlays]).
