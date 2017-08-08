%%%-------------------------------------------------------------------
%% @doc etspersist public API
%% @end
%%%-------------------------------------------------------------------

-module(etspersist_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    etspersist_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
