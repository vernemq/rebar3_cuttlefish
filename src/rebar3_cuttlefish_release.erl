-module(rebar3_cuttlefish_release).
-behaviour(provider).

-export([init/1
        ,do/1
        ,format_error/1]).

-define(PROVIDER, release).
-define(NAMESPACE, cuttlefish).
-define(DEPS, [{default, compile}]).

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
            {example, "rebar3 cuttlefish release"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Rebar3 cuttlefish release plugin"},
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

    Deps = rebar_state:all_deps(State),
    Apps = rebar_state:project_apps(State),
    {ok, Cuttlefish} = rebar_app_utils:find(<<"cuttlefish">>, rebar_state:all_plugin_deps(State)),

    AllSchemas = schemas([Cuttlefish | Deps++Apps]),

    case cuttlefish_schema:files(AllSchemas) of
        {errorlist, _Es} ->
            %% These errors were already printed
            {error, "bad cuttlefish schemas"};
        {_Translations, Mappings, _Validators} ->
            Overlays1 = case lists:keyfind(overlay, 1, Relx) of
                            {overlay, Overlays} when is_list(Overlays) ->
                                Overlays ++ overlays(Name, Overlays, AllSchemas);
                            _ ->
                                overlays(Name, [], AllSchemas)
                        end,

            ConfFile = filename:join("config", atom_to_list(Name)++".conf"),
            Overlays2 = case filelib:is_regular(ConfFile) of
                            true ->
                                [{copy, ConfFile, "etc/"} | Overlays1];
                            false ->
                                make_default_file(Name, TargetDir, Mappings),
                                Overlays1
                        end,

            State1 = rebar_state:set(State, relx, [{sys_config, false},
                                                   {vm_args, false},
                                                   {generate_start_script, false},
                                                   {overlay, Overlays2} | lists:keydelete(overlay, 1, Relx)]),
            rebar_relx:do(rlx_prv_release, "release", ?PROVIDER, State1)
    end.

-spec format_error(any()) ->  iolist().
format_error(Error) ->
    io_lib:format("~p", [Error]).

make_default_file(Name, TargetDir, Mappings) ->
    File = io_lib:format("~s.conf", [Name]),
    Filename = filename:join([TargetDir, "etc", File]),
    filelib:ensure_dir(Filename),
    cuttlefish_conf:generate_file(Mappings, Filename).

schemas(Apps) ->
    lists:flatmap(fun(App) ->
                      Dir = rebar_app_info:dir(App),
                      filelib:wildcard(filename:join([Dir, "{priv,schema}", "*.schema"]))
                  end, Apps) ++ filelib:wildcard(filename:join(["{priv,schema}", "*.schema"])).

overlays(Name, Overlays, Schemas) ->
    BinScriptTemplate = filename:join([code:priv_dir(rebar3_cuttlefish), "bin_script"]),
    NodeTool = filename:join([code:priv_dir(rebar3_cuttlefish), "nodetool"]),
    InstallUpgrade = filename:join([code:priv_dir(rebar3_cuttlefish), "install_upgrade_escript"]),
    BinScript = filename:join(["bin", Name]),
    SchemaOverlays = [{template, Schema, filename:join(["share", "schema", filename:basename(Schema)])}
                     || Schema <- Schemas, not lists:keymember(Schema, 2, Overlays)],
    [{copy, "./_build/default/bin/cuttlefish", "bin/cuttlefish"},
     {mkdir, "share"},
     {mkdir, "share/schema"},
     {copy, NodeTool, filename:join(["bin", "nodetool"])},
     {copy, InstallUpgrade, filename:join(["bin", "install_upgrade.escript"])},
     {template, BinScriptTemplate, BinScript} | SchemaOverlays].
