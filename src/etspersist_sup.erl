%%%-------------------------------------------------------------------
%% @doc etspersist top level supervisor.
%%%-------------------------------------------------------------------

-module(etspersist_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_Args) -> Server =
  {etspersist_srv,
    {etspersist_srv, start_link, []},
    permanent, 5000, worker,
    dynamic % XXX
  },
  {ok, {{one_for_one, 3, 10}, [Server]}}.

%%====================================================================
%% Internal functions
%%====================================================================
