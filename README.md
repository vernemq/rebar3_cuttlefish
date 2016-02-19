rebar3_cuttlefish
=====

This plugin provides cuttlefish and automatically updates the release configuration at runtime to utilize cuttlefish.

Add the plugin to your top level rebar config:

    {plugins, [rebar3_cuttlefish]}.


The cuttlefish release and tar providers need

    $ rebar3 cuttlefish release

The plugin provider will tell relx to create the dirs `share/schema` in the release, copy the cuttlefish escript to `bin/` and discover all schema files in the project apps and their dependencies.

Since cuttlefish uses sort order of the schema files by their name you may want to prepend a number to the schema file name, like `01-eleveldb.schema`. While the plugin will automatically copy all schema files for you it will first check if there is already an overlay entry for each schema file. So if in `overlay` there is an entry `{template, "schema/eleveldb.schema", "01-eleveldb.schema"}` it will not make another copy.
