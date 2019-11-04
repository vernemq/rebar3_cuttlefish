-module(rebar3_cuttlefish_release).
-behaviour(provider).

-export([init/1
        ,do/1
        ,format_error/1]).

-define(PROVIDER, release).
-define(NAMESPACE, default).
-define(DEPS, [{?NAMESPACE, compile}]).
-define(SCHEMA_IDX_START, 10).

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

    ReleaseDir = rebar_release_dir(State),
    CuttlefishBin = case filelib:is_regular(filename:join([rebar_dir:base_dir(State), "bin", "cuttlefish"])) of
                 true ->
                     filename:join([rebar_dir:base_dir(State), "bin", "cuttlefish"]);
                 false ->
                     case filelib:wildcard(filename:join(["_checkouts", "cuttlefish*", "cuttlefish"])) ++
                         filelib:wildcard(filename:join(["_build", "*", "bin", "cuttlefish"])) of
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

    OrderSchemas = case lists:keyfind(schema_order, 1, CFConf) of
                       {schema_order, Order} -> Order;
                       _ -> false
                   end,

    DisableCFRelScripts = case lists:keyfind(disable_bin_scripts, 1, CFConf) of
                              {disable_bin_scripts, true} -> true;
                              _ -> false
                          end,

    Overlays1 = case {lists:keyfind(schema_discovery, 1, CFConf),
                      lists:keyfind(overlay, 1, Relx)} of
                    {{schema_discovery, false}, {overlay, Overlays}} ->
                        overlays(Name, CuttlefishBin, Overlays, [], OrderSchemas) ++ Overlays;
                    {{schema_discovery, false}, _} ->
                        overlays(Name, CuttlefishBin, [], [], OrderSchemas);
                    {_, {overlay, Overlays}} when is_list(Overlays) ->
                        overlays(Name, CuttlefishBin, Overlays, AllSchemas, OrderSchemas) ++ Overlays;
                    _ ->
                        overlays(Name, CuttlefishBin, [], AllSchemas, OrderSchemas)
                end,

    Overlays2 = overlay_add_bin_scripts(DisableCFRelScripts, Name, Overlays1),

    ConfFile = filename:join("config", atom_to_list(Name)++".conf"),

    Overlays3 = case filelib:is_regular(ConfFile) of
                    true ->
                        [{template, ConfFile, "etc/" ++ CFFile} | Overlays2];
                    false ->
                        [{mkdir "etc/"} | Overlays2]
                end,

    StartHookState = maybe_set_startup_hook(DisableCFRelScripts, State),
    State1 = rebar_state:set(State, relx, lists:keydelete(overlay, 1, Relx) ++
                                 [{generate_start_script, DisableCFRelScripts},
                                  {overlay, Overlays3} | StartHookState]),
    Res = rebar_relx:do(rlx_prv_release, "release", ?PROVIDER, State1),
    SchemaGlob = filename:join([TargetDir, "share", "schema", "*.schema"]),
    ReleaseSchemas = filelib:wildcard(SchemaGlob),

    case filelib:is_regular(ConfFile) of
        false ->
            case cuttlefish_schema:files(ReleaseSchemas) of
                {errorlist, _Es} ->
                    %% These errors were already printed
                    {error, "bad cuttlefish schemas"};
                {_Translations, Mappings, _Validators} ->
                    make_default_file(CFFile, TargetDir, Mappings),
                    Res
            end;
        true ->
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

overlays(_Name, Cuttlefish, Overlays, Schemas, OrderSchemas) ->
    Overlays1 = [ list_to_binary(F) || {_, F, _} <- Overlays],
    SchemaOverlays = [begin
                            FileName = create_filename(Schema, OrderSchemas),
                            {template, Schema, filename:join(["share", "schema", FileName])}
                      end || Schema <- Schemas, not is_overlay(Schema, Overlays1)],
    [{copy, Cuttlefish, "bin/cuttlefish"},
     {mkdir, "share"},
     {mkdir, "share/schema"} | SchemaOverlays].

create_filename(Schema, false) ->
    filename:basename(Schema);
create_filename(Schema, OrderSchemas) ->
    SchemaBaseName = list_to_atom(filename:basename(Schema, ".schema")),
    IndexOrderSchemas = lists:zip(OrderSchemas, lists:seq(?SCHEMA_IDX_START, length(OrderSchemas) + (?SCHEMA_IDX_START - 1))),
    Index = lists:keyfind(SchemaBaseName, 1, IndexOrderSchemas),
    maybe_index_filename(Schema, Index).

maybe_index_filename(Schema, false) ->
    filename:basename(Schema);
maybe_index_filename(Schema, {_, Index}) ->
    integer_to_list(Index) ++ "-" ++ filename:basename(Schema).

overlay_add_bin_scripts(true, _Name, Overlays) ->
    CFConfigHookTemplate = filename:join([code:priv_dir(rebar3_cuttlefish), "cf_config"]),
    [{template, CFConfigHookTemplate, filename:join(["bin", "cf_config"])} | Overlays];
overlay_add_bin_scripts(false, Name, Overlays) ->
    BinScriptTemplate = filename:join([code:priv_dir(rebar3_cuttlefish), "bin_script"]),
    NodeTool = filename:join([code:priv_dir(rebar3_cuttlefish), "nodetool"]),
    InstallUpgrade = filename:join([code:priv_dir(rebar3_cuttlefish), "install_upgrade_escript"]),
    BinScript = filename:join(["bin", Name]),
    [{copy, NodeTool, filename:join(["bin", "nodetool"])},
     {copy, InstallUpgrade, filename:join(["bin", "install_upgrade.escript"])},
     {template, BinScriptTemplate, BinScript} | Overlays].

maybe_set_startup_hook(false, _State) ->
    [{sys_config, false}, {vm_args, false}];
maybe_set_startup_hook(true, State) ->
    RelxState = rebar_state:get(State, relx),
    StartHooks0 = 
        case lists:keyfind(extended_start_script_hooks, 1, RelxState) of
            {extended_start_script_hooks, StartHooks1} ->
                do_set_start_hook(StartHooks1);
            _ ->
                do_set_start_hook([])
        end,
    [{extended_start_script, true},
     {extended_start_script_hooks, StartHooks0}].

do_set_start_hook(StartHooks) ->
    CFPreStart = {custom, "cf_config"},
    PreStart =
        case lists:keyfind(pre_start, 1, StartHooks) of
            {pre_start, PreStartHooks} ->
                [CFPreStart | PreStartHooks];
            _ ->
                [CFPreStart]
        end,
    lists:keystore(pre_start, 1, StartHooks, {pre_start, PreStart}).

is_overlay(SchemaS, Overlays) ->
    Schema = list_to_binary(SchemaS),
    Suffixes = [{byte_size(Overlay),
                 binary:longest_common_suffix(
                   [Schema, Overlay])} || Overlay <- Overlays],
    [L || {L, L} <- Suffixes] =/= [].

%% Determine if --output-dir or -o has been passed, and if so, use
%% that path for the release directory. Otherwise, default to the
%% default directory.
rebar_release_dir(State) ->
    DefaultRelDir = filename:join(rebar_dir:base_dir(State), "rel"),
    {Options, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(output_dir, Options) of
        undefined ->
            DefaultRelDir;
        OutputDir ->
            OutputDir
    end.
