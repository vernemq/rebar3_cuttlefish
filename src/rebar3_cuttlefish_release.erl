-module(rebar3_cuttlefish_release).
-behaviour(provider).

-export([init/1
        ,do/1
        ,format_error/1]).

-define(PROVIDER, release).
-define(NAMESPACE, default).
-define(DEPS, [{?NAMESPACE, compile}]).

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
            {opts, relx:opt_spec_list()},                   % list of options understood by the plugin
            {short_desc, "Rebar3 cuttlefish release plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running cuttlefish schema generator", []),
    Relx = rebar_state:get(State, relx, []),
    CFConf = rebar_state:get(State, cuttlefish, []),

    ReleaseDir = filename:join(rebar_dir:base_dir(State), "rel"),
    CuttlefishBin = case filelib:is_regular(filename:join([rebar_dir:base_dir(State), "bin", "cuttlefish"])) of
                 true ->
                     filename:join([rebar_dir:base_dir(State), "bin", "cuttlefish"]);
                 false ->
                     case filelib:wildcard(filename:join(["_build", "*", "bin", "cuttlefish"])) of
                         [C | _] ->
                             C;
                         [] ->
                             throw({no_cuttlefish_escript, rebar_dir:base_dir(State)})
                     end
             end,


    {release, {Name, _Vsn}, _} = lists:keyfind(release, 1, Relx),
    CFFile = case lists:keyfind(file_name, 1, CFConf) of
                   {file_name, DefinedFileName} ->
                       DefinedFileName;
                   _ ->
                       io_lib:format("~s.conf", [Name])
               end,
    TargetDir = filename:join([ReleaseDir, Name]),
    Deps = rebar_state:all_deps(State),
    Apps = rebar_state:project_apps(State),
    {ok, Cuttlefish} = rebar_app_utils:find(<<"cuttlefish">>, rebar_state:all_plugin_deps(State)),

    AllSchemas = schemas([Cuttlefish | Deps++Apps]),

    Overlays1 = case {lists:keyfind(schema_discovery, 1, CFConf),
                      lists:keyfind(overlay, 1, Relx)} of
                    {{schema_discovery, false}, {overlay, Overlays}} ->
                        Overlays ++ overlays(Name, CuttlefishBin, Overlays, []);
                    {{schema_discovery, false}, _} ->
                        overlays(Name, CuttlefishBin, [], []);
                    {_, {overlay, Overlays}} when is_list(Overlays) ->
                        Overlays ++ overlays(Name, CuttlefishBin, Overlays, AllSchemas);
                    _ ->
                        overlays(Name, CuttlefishBin, [], AllSchemas)
                end,
    ConfFile = filename:join("config", atom_to_list(Name)++".conf"),

    Overlays2 = case filelib:is_regular(ConfFile) of
                    true ->
                        [{template, ConfFile, "etc/" ++ CFFile} | Overlays1];
                    false ->
                        Overlays1
                end,
    State1 = rebar_state:set(State, relx, lists:keydelete(overlay, 1, Relx) ++
                                 [{sys_config, false},
                                  {vm_args, false},
                                  {generate_start_script, false},
                                  {overlay, Overlays2}]),
    Res = rebar_relx:do(rlx_prv_release, "release", ?PROVIDER, State1),
    SchemaGlob = filename:join([TargetDir, "share", "schema", "*.schema"]),
    ReleaseSchemas = filelib:wildcard(SchemaGlob),
    case cuttlefish_schema:files(ReleaseSchemas) of
        {errorlist, _Es} ->
            %% These errors were already printed
            {error, "bad cuttlefish schemas"};
        {_Translations, Mappings, _Validators} ->
            make_default_file(CFFile, TargetDir, Mappings),
            Res
    end.

-spec format_error(any()) ->  iolist().
format_error({no_cuttlefish_escript, ProfileDir}) ->
    io_lib:format("No cuttlefish escript found under ~s or ~s", [filename:join(ProfileDir, "bin"),
                                                                "_build/default/bin"]);
format_error(Error) ->
    io_lib:format("~p", [Error]).

make_default_file(File, TargetDir, Mappings) ->
    Filename = filename:join([TargetDir, "etc", File]),
    filelib:ensure_dir(Filename),
    cuttlefish_conf:generate_file(Mappings, Filename),
    Filename.

schemas(Apps) ->
    lists:flatmap(fun(App) ->
                      Dir = rebar_app_info:dir(App),
                      filelib:wildcard(filename:join([Dir, "{priv,schema}", "*.schema"]))
                  end, Apps) ++ filelib:wildcard(filename:join(["{priv,schema}", "*.schema"])).

overlays(Name, Cuttlefish, Overlays, Schemas) ->
    BinScriptTemplate = filename:join([code:priv_dir(rebar3_cuttlefish), "bin_script"]),
    NodeTool = filename:join([code:priv_dir(rebar3_cuttlefish), "nodetool"]),
    InstallUpgrade = filename:join([code:priv_dir(rebar3_cuttlefish), "install_upgrade_escript"]),
    BinScript = filename:join(["bin", Name]),
    Overlays1 = [ list_to_binary(F) || {_, F, _} <- Overlays],
    SchemaOverlays = [{template, Schema, filename:join(["share", "schema", filename:basename(Schema)])}
                      || Schema <- Schemas, not is_overlay(Schema, Overlays1)],
    [{copy, Cuttlefish, "bin/cuttlefish"},
     {mkdir, "share"},
     {mkdir, "share/schema"},
     {copy, NodeTool, filename:join(["bin", "nodetool"])},
     {copy, InstallUpgrade, filename:join(["bin", "install_upgrade.escript"])},
     {template, BinScriptTemplate, BinScript} | SchemaOverlays].

is_overlay(SchemaS, Overlays) ->
    Schema = list_to_binary(SchemaS),
    Suffixes = [{byte_size(Overlay),
                 binary:longest_common_suffix(
                   [Schema, Overlay])} || Overlay <- Overlays],
    [L || {L, L} <- Suffixes] =/= [].
