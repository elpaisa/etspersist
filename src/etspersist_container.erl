%%%-------------------------------------------------------------------
%%% @author johnleytondiaz
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2017 10:30 AM
%%%-------------------------------------------------------------------
-module(etspersist_container).
-author("johnleytondiaz").
-behaviour(gen_server).
-author("johnleytondiaz").

-define(MAX_TIMEOUT, 1000).

-ifdef(TEST).
-compile([export_all]).
-endif.

-export([start_link/1]).

%% server functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link(_) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @private
init(_Args) ->
  {ok, #state{}}.

handle_call(_Request, _, State) ->
  {noreply, ok, State}.
handle_cast(_, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(normal, _State) ->
  ok;
terminate(shutdown, _State) ->
  ok;
terminate({shutdown, _}, _State) ->
  ok;
terminate(_Crash, State) ->
  gen_server:cast(?MODULE, {survive, State}),
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
