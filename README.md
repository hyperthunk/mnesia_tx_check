## Running/Installing

You'll need an up to date copy of rebar on your system - I'd recommend at
least 2.0.0 but actually fetching and installing from basho/rebar master
will also work fine at the time of writing (October 9th, 2012).

```
$ git clone git://github.com/hyperthunk/mnesia_tx_check.git
$ cd mnesia_tx_check
$ rebar get-deps compile
$ rebar skip_deps=true systest
```

If you want to see additional logging info in the console, you can run rebar
in *verbose* mode by adding `-v 4` to that last command.

## Hacking

A six node cluster is configured in `./resources/nodes.resource`, where each
node is started using the [slave](http://www.erlang.org/doc/man/slave.html)
module. The cluster `on_start` hooks bootstraps mnesia for all the nodes,
which the slave/node `on_join` hook triggers the mnesia_tx_check application
startup.

