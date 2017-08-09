%%%-------------------------------------------------------------------
%%% @author John Diaz
%%% @copyright (C) 2017, elpaisa
%%% @doc
%%% Empty gen_server container to assign ETS tables
%%% This will be supervised and restarted whenever an error
%%% occurs.
%%% @end
%%% Created : 08. Aug 2017 10:30 AM
%%%-------------------------------------------------------------------
-module(etspersist_container).
-author("John Diaz").
-behaviour(gen_server).
-author("John Diaz").

-define(MAX_TIMEOUT, 1000).

-ifdef(TEST).
-compile([export_all]).
-endif.

-export([start_link/1]).

%% server functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link(ETS) ->
  gen_server:start_link({local, ETS}, ?MODULE, [], []).


%% @private
init(_Args) ->
  {ok, #state{}}.

handle_call(_Request, _, State) ->
  {noreply, ok, State}.
handle_cast(crash, State) ->
  erlang:error(badarg),
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_info({'ETS-TRANSFER', Tab, _, TName}, _State) ->
  utils:debug("Got ownership for: ~p.", [TName]),
  {noreply, Tab};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(normal, _State) ->
  ok;
terminate(shutdown, _State) ->
  ok;
terminate({shutdown, _}, _State) ->
  ok;
terminate(_Crash, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
