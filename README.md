rebar3_cuttlefish
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_cuttlefish, ".*", {git, "git@host:user/rebar3_cuttlefish.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_cuttlefish
    ===> Fetching rebar3_cuttlefish
    ===> Compiling rebar3_cuttlefish
    <Plugin Output>
