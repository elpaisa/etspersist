[Build Status / link to build job](#)

# etspersist

Erlang OTP19 ETS Persistence module

## Overview

This module keeps the state of ETS tables and allows to manage data from it.

Please see project definition [documentation][api-documentation]

## Ownership

etspersist is owned by the [elpaisa][email]. Feel free to [email me][email]."


## How to contribute

Always submit issues for discussion before creating any pull requests.

## How to report defects

Open github issues

## Running locally

For running locally please use.

```sh
git clone git@github.com:elpaisa/etspersist.git.git
cd etspersist
make shell
```

etspersist requires configuration parameters. Copy the 
`sys.config.tmpl` as `config/sys.config` and edit it accordingly:

```sh
cp sys.config.tmpl config/sys.config
vim sys.config
```

```erlang
%%-*- mode: erlang -*-
[
  {etspersist, [
    {timeout_ms, 5},
    {max_timeout_ms, 1000}
  {sasl, [
    {sasl_error_logger, false}
  ]}
].
% vim:ft=erlang

```

Once you have filled out the above changes to the `sys.config`, you can start
etspersist:

```sh
make shell
```

Or

```
./rebar3 shell
```

### Testing

To run the tests, execute:

```sh
make test
```

#### Unit Tests

To run the unit tests alone, execute:

```sh
make eunit
```

OR 

```
./rebar3 eunit
```


If there is something unique about the eunit configuration, or the output of the tests or code
coverage are sent somewhere different than conventional, describe here how to view those reports.

#### Common tests

To run the common tests alone, execute:

```sh
make ct
```

[design-doc]: #
[email]: mailto:clientes@desarrollowebmedellin.com
[api-documentation]: https://elpaisa.github.io/etspersist/
[apidoc]: http://apidocjs.com

<!--- vim: sw=4 et ts=4 -->
