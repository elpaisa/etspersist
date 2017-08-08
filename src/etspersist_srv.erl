%%%-------------------------------------------------------------------
%%% @author johnleytondiaz
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2017 10:30 AM
%%%-------------------------------------------------------------------
-module(etspersist_srv).
-author("johnleytondiaz").
-behaviour(gen_server).
-author("johnleytondiaz").

-define(MAX_TIMEOUT, 1000).

-ifdef(TEST).
-compile([export_all]).
-endif.

-export([start_link/0]).

%% server functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([new_ets/1]).
-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @private
init(_Args) ->
  utils:inf("Initializing ETS Persistence Module for ~p", [?MODULE]),
  observer:start(),
  {ok, #state{}}.

handle_call({new_ets, ETS}, _, State) ->
  start_child(ETS),
  {reply, give_away(ETS, create_table(ETS)), State};
handle_call(_Request, _, State) ->
  {noreply, ok, State}.
handle_cast(_, State) ->
  {noreply, State}.

handle_info({'ETS-TRANSFER', Id, _, ETS}, State) ->
  give_away(ETS, Id),
  {noreply, State};
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

give_away(ETS, TId) ->
  give_away(ETS, TId, await(ETS)).

give_away(_, _, timeout) ->
  {error, timeout};
give_away(ETS, Id, Pid) when is_pid(Pid) ->
  utils:inf("Reassign ETS ~p to ~p", [ETS, Pid]),
  ets:give_away(Id, Pid, ETS),
  {ok, Pid};
give_away(_, _, Response) ->
  utils:err("Unexpected response from handler ~p", [Response]).

await(ETS)->
  await(ETS, 10).

await(ETS, Timeout) ->
  await(ETS, Timeout, utils:is_alive(ETS)).

await(_, Timeout, false) when Timeout >= ?MAX_TIMEOUT ->
  timeout;
await(ETS, Timeout, false) ->
  timer:sleep(Timeout),
  await(ETS, Timeout + 10, utils:is_alive(ETS));
await(_, _, Pid) ->
  Pid.

new_ets(ETS)->
  gen_server:call(?MODULE, {new_ets, ETS}, infinity).


create_table(ETS)->
  Options = [set, named_table, public, compressed, {heir,self(),ETS}],
  ets:new(ETS, Options).

start_child(Name)->
  Worker = {Name,{etspersist_container,start_link,[Name]},
    permanent,2000,worker,[etspersist_container]},
  {ok, Pid} = supervisor:start_child(etspersist_sup, Worker),
  register(Name, Pid).