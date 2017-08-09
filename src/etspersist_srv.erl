%%%-------------------------------------------------------------------
%%% @author John Diaz
%%% @copyright (C) 2017, elpaisa
%%% @doc
%%% Table manager module
%%% Takes care of child starting when a new ETS table is created and
%%% sets the heir of it to the newly started child
%%% @end
%%% Created : 08. Aug 2017 10:30 AM
%%%-------------------------------------------------------------------
-module(etspersist_srv).
-author("John Diaz").
-behaviour(gen_server).

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
  {ok, #state{}}.

handle_call({new_ets, ETS}, _, State) ->
  Pid = start_child(ETS),
  R = give_away(ETS, create_table(ETS), Pid),
  {reply, R, State};
handle_call(_Request, _, State) ->
  {noreply, ok, State}.
handle_cast(_, State) ->
  {noreply, State}.

handle_info({'ETS-TRANSFER', Table, _, ETS}, State) ->
  give_away(ETS, Table, is_alive(ETS)),
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

give_away(ETS, Id, false) ->
  give_away(ETS, Id, await(ETS));
give_away(ETS, Id, Pid) when is_pid(Pid) ->
  utils:debug("Reassign ETS ~p to ~p", [ETS, Pid]),
  ets:give_away(Id, Pid, ETS),
  {ok, Pid};
give_away(_, _, R) ->
  utils:err("Unexpected response from handler ~p", [R]).

new_ets(ETS) ->
  gen_server:call(?MODULE, {new_ets, ETS}, infinity).

create_table(ETS) ->
  Options = [set, named_table, public, compressed, {heir, self(), ETS}],
  ets:new(ETS, Options).

start_child(Name0) ->
  Name = get_name(Name0),
  Worker = {Name, {etspersist_container, start_link, [Name]},
    permanent, 2000, worker, [etspersist_container]},
  {ok, Pid} = supervisor:start_child(etspersist_sup, Worker),
  Pid.

await(ETS) ->
  Timeout = etspersist:get_env(timeout_ms, 5),
  MaxTimeout = etspersist:get_env(max_timeout_ms, 1000),
  await(ETS, Timeout, MaxTimeout).

await(ETS, Timeout, MaxTimeout) ->
  await(is_alive(ETS), ETS, Timeout, MaxTimeout).

await(false, _, Timeout, MaxTimeout) when Timeout >= MaxTimeout ->
  timeout;
await(false, ETS, Timeout, MaxTimeout) ->
  timer:sleep(Timeout),
  await(is_alive(ETS), ETS, (Timeout + Timeout), MaxTimeout);
await(Pid, _, _, _) ->
  Pid.

is_alive(ETS)->
  utils:is_alive(get_name(ETS)).

get_name(Name)->
  Prefix = etspersist:get_env(proc_prefix, ?MODULE),
  utils:atom_join([Prefix, Name], "_").