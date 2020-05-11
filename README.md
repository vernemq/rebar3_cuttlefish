rebar3_cuttlefish
=====

This plugin provides cuttlefish and automatically updates the release configuration at runtime to utilize cuttlefish. There is no need to add schema files to the relx overlay, copy the cuttlefish escript or create a custom start script, it is all handled by the plugin provider.

Add the plugin to your top level rebar config under `project_plugins` so it can override the default `release` and `tar` providers:

    {project_plugins, [rebar3_cuttlefish]}.


Now the cuttlefish release and tar providers will be run when the `release` or `tar` task are run:

    $ rebar3 release

The plugin provider will tell relx to create the dirs `share/schema` in the release, copy the cuttlefish escript to `bin/` and discover all schema files in the project apps and their dependencies. All you should need for your relx config is:

```erlang
{relx, [{release, {<name>, "0.1.0"},
        [<app_name>]},

        {dev_mode, true},
        {include_erts, false},

        {overlay_vars, "config/vars.config"}]}.
```

```erlang
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

%% Platform-specific installation paths
{platform_data_dir, "./data"}.

%% etc/vm.args
{node,         "<name>@127.0.0.1"}.
{crash_dump,   "{{platform_log_dir}}/erl_crash.dump"}.
```

## Configuration

The Cuttlefish plugin can take some configuration values:
```erlang
{cuttlefish,
 [{file_name, "<release>.conf.example"},
  {schema_discovery, false}]}
```

- `file_name` will define the file the config is written to. (`<release>.conf` is the default)
- `schema_discovery` can be set to true or false to either enable or disalbe discovering all schema files in libraries. (`true` is the default).

## Running Multiple Nodes

Somtimes for testing you want to boot multiple nodes and require separate node names or ports, rebar3 makes this easy. First add a profile entry per node you want to run:

```erlang
{profiles, [
    {node1, [{relx, [{overlay_vars, ["config/vars.config", "config/vars_node1.config"]}]}]}
    {node2, [{relx, [{overlay_vars, ["config/vars.config", "config/vars_node2.config"]}]}]}
    {node2, [{relx, [{overlay_vars, ["config/vars.config", "config/vars_node2.config"]}]}]}
]}.
```

For each profile create the `config/vars_node<#>.config`

```erlang
{node, "node<#>@127.0.0.1"}.
```

Build each separately:

```shell
$ rebar3 as node<#> release
```

Run separately:

```shell
$ _build/node<#>/rel/<name>/bin/<name> console
```

## Optional Ordering of Schemas

Since cuttlefish uses sort order of the schema files by their name you may want to prepend a number to the schema file name, like `01-eleveldb.schema`. While the plugin will automatically copy all schema files for you it will first check if there is already an overlay entry for each schema file. So if in `overlay` there is an entry `{template, "schema/eleveldb.schema", "01-eleveldb.schema"}` it will not make another copy.

Alternatively, we can specify the ordering of discovered schemas in the rebar.config by setting schema_order in our cuttlefish configuration. This will handle the prefixing for you and the discovered schemas will be ordered as per the order specified in the list. Any discovered schemas that are not in the list will be copied over as-is (with no ordering prefix).

```erlang
{cuttlefish,
 [{file_name, "<release>.conf.example"},
  {schema_discovery, true},
  {schema_order, [
    riak
    erlang_vm
    riak_core
  ]}
]}.
```
Discovered schemas:
```erlang
riak.schema
erlang_vm.schema
riak_core.schema
riak_repl.schema
```

Resulting copied schemas:
```erlang
10-riak.schema
11-erlang_vm.schema
12-riak_core.schema
riak_repl.schema (not specified in ordering)
```
