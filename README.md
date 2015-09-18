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
        {rebar3_cuttlefish, {git, "git@github.com:tsloughter/rebar3_cuttlefish.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 cuttlefish
    ===> Fetching rebar3_cuttlefish
    ===> Compiling rebar3_cuttlefish
    ===> Running cuttlefish schema generator
