[Build Status / link to build job](#)

# db_flagship

Erlang OTP19 Mysql Connection manager with a self managed pool

## Overview

This module allows to reuse connections to mysql using worker names.

Please see project definition [documentation][api-documentation]

## Ownership

db_flagship is owned by the [elpaisa][email]. Feel free to [email me][email]."

## Dependencies

This service depends on Mysql OTP


## How to consume db_flagship

## How to contribute

Always submit issues for discussion before creating any pull requests.

## How to report defects

Open github issues

## Running locally

For running locally please use.

```sh
git clone git@github.com:elpaisa/db_flagship.git.git
cd db_flagship
make shell
```

db_flagship requires configuration parameters. Copy the 
`sys.config.tmpl` as `config/sys.config` and edit it accordingly:

```sh
cp sys.config.tmpl config/sys.config
vim sys.config
```

```erlang
%%-*- mode: erlang -*-
[
  {db_flagship, [
    {connections, [
       {conn1, [
        {host, "localhost"},
        {user, "root"},
        {pass, "0000"}
       ]}
    ]}
  {sasl, [
    {sasl_error_logger, false}
  ]}
].
% vim:ft=erlang

```

Once you have filled out the above changes to the `sys.config`, you can start
db_flagship:

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
[api-documentation]: https://elpaisa.github.io/db_flagship/
[apidoc]: http://apidocjs.com

<!--- vim: sw=4 et ts=4 -->
