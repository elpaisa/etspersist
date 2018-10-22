%%%-------------------------------------------------------------------
%%% @author John Diaz
%%% @copyright (C) 2017, elpaisa
%%% @doc
%%% Module for ETS persistence
%%% Handles ETS tables for most common operations, insert, get, update
%%% whenever an operation is done for any table, this one can or not
%%% exists, it will create it automatically using a gen_server, ETS
%%% tables will be created independent of the caller process, if this
%%% one dies, they will persist along with their data.
%%% @end
%%% Created : 08. Aug 2017 1:43 PM
%%%-------------------------------------------------------------------
-module(etspersist).
-author("John Diaz").

-export([test/0]).

%% API
-export([new/1, get/2, get/3, take/2, take/3, insert/2, insert/3, update/3, delete/2, delete/3, call/2]).
-export([get_env/1, get_env/2, set_env/2, is_alive/1, atom_join/1]).

test() ->
  new(ets_test1),
  insert(ets_test1, {1, good}),
  new(ets_test2),
  gen_server:cast(ets_test1, crash),
  timer:sleep(2000),
  ets:info(ets_test1),
  %%get(ets_test1, 1),
  call(get, [ets_test1, 1]).

-spec new(ETS :: atom()) -> tuple().
%%
%% @equiv new(ETS, ets:info(ETS))
%%
new(ETS) ->
  new(ETS, ets:info(ETS)).

-spec new(ETS :: atom(), term()) -> tuple().
%%
%% @doc Creates an ets table by name
%% @param ETS name of the table to create
%% @param ETS info
%%
new(ETS, undefined) ->
  etspersist_srv:new_ets(ETS);
new(_, Table) ->
  Table.

-spec get(ETS :: atom(), Key :: any()) -> {ok, Table :: tuple(), any()}.
%%
%% @doc Gets an item from an ets
%% @equiv get(Table, ETS, Key)
%% @param ETS name of the table to get the key from
%% @param Key key name to get from the table
%%
get(ETS, Key) ->
  get(new(ETS), ETS, Key).

-spec get(Table :: tuple(), ETS :: atom(), Key :: any()) -> {ok, Table :: tuple(), any()}.
%%
%% @doc Gets an item from an ets
%% @param Table actual ETS table
%% @param ETS name of the table to get the key from
%% @param Key key name to get from the table
%%
get(Table, ETS, Key) ->
  Search = ets:lookup(ETS, Key),
  {ok, Table, Search}.

-spec take(ETS :: atom(), Key :: any()) -> {ok, Table :: tuple(), any()}.
%%
%% @doc Takes an item from an ets, that means the key will be deleted as soon as it
%% is retrieved
%% @equiv take(Table, ETS, Key)
%% @param ETS name of the table to get the key from
%% @param Key key name to get from the table
%%
take(ETS, Key) ->
  take(new(ETS), ETS, Key).

-spec take(Table :: tuple(), ETS :: atom(), Key :: any()) -> {ok, Table :: tuple(), any()}.
%%
%% @doc Takes an item from an ets
%% @param Table actual ETS table
%% @param ETS name of the table to take the key from
%% @param Key key name to get from the table
%%
take(Table, ETS, Key) ->
  Take = ets:take(ETS, Key),
  {ok, Table, Take}.


-spec insert(ETS :: atom(), Key :: any()) -> {ok, Table :: tuple(), any()}.
%%
%% @doc Inserts an item into an ets
%% @equiv insert(Table, ETS, Value)
%%
insert(ETS, Value) ->
  insert(new(ETS), ETS, Value).

-spec insert(Table :: tuple(), ETS :: atom(), Key :: any()) -> {ok, Table :: tuple(), any()}.
%%
%% @doc Inserts an item into an ets
%% @param Table actual ETS table
%% @param ETS name of the table to insert the record
%% @param Value actual record to be inserted
%%
insert(Table, ETS, Value) ->
  Insert = ets:insert(ETS, Value),
  {ok, Table, Insert}.

-spec update(ETS :: atom(), Position :: integer(), Value :: any()) -> {ok, Table :: tuple(), any()}.
%%
%% @doc Updates an item in an ets
%% @equiv update(Table, ETS, Position, Value)
%%
update(ETS, Position, Value) ->
  update(new(ETS), ETS, Position, Value).

-spec update(Table :: tuple(), ETS :: atom(), Position :: integer(), Value :: any()
) -> {ok, Table :: tuple(), any()}.
%%
%% @doc Updates an item in an ets by its position in the record
%%
%% @param Table actual ETS table
%% @param ETS name of the table to insert the record
%% @param Position position of the record to update 2 = {First, "Second"}
%% @param Value value to set in the specified position
%%
update(Table, ETS, Position, Value) ->
  Update = ets:update_element(ETS, Position, Value),
  {ok, Table, Update}.

-spec delete(ETS :: atom(), Key :: any()) -> {ok, Table :: tuple(), term()}.
%%
%% @doc Updates an item in an ets
%% @equiv delete(Table, ETS, Key)
%%
delete(ETS, Key) ->
  delete(new(ETS), ETS, Key).

-spec delete(Table :: term(), ETS :: atom(), Key :: any()) -> {ok, Table :: tuple(), term()}.
%%
%% @doc Deletes an item from an ets by its key
%%
%% @param Table actual ETS table
%% @param ETS name of the table to insert the record
%% @param Key key of the record
%%
delete(Table, ETS, Key) ->
  Delete = ets:delete(ETS, Key),
  {ok, Table, Delete}.

-spec call(F :: atom(), Params :: term()) -> list().
%%
%% @doc Execs a function against a table
%%
%% @param Function function to be exec
%% @param Params list ETS and others
%%
call(insert, Params)->
  response(apply(?MODULE, insert, Params));
call(update, Params)->
  response(apply(?MODULE, update, Params));
call(take, Params)->
  response(apply(?MODULE, take, Params));
call(get, Params)->
  response(apply(?MODULE, get, Params));
call(F, _)->
  error_logger:info_msg("Undefined function ~p", [F]),
  [].

response({ok, _Table, R})->
  R;
response(R)->
  error_logger:error_msg("Unhandled response ~p", [R]),
  [].

-spec get_env(atom()) -> term() | 'undefined'.
%%
%% @doc get an environment variable's value (or undefined if it doesn't exist)
%%
get_env(Key) ->
  get_env(Key, 'undefined').


-spec get_env(atom(), term()) -> term().
%%
%% @doc get an environment variable's value (or Default if it doesn't exist)
%%
get_env(Key, Default) ->
  application:get_env(?MODULE, Key, Default).


-spec set_env(atom(), any()) -> ok.
%%
%% @doc set the environment variable's value
%%
set_env(Key, Value) ->
  application:set_env(?MODULE, Key, Value).


-spec is_alive(Name :: atom()) -> pid() | false.
%%
%% @doc Seeks a Process by its name and gets its Pid, if not running returns false,
%% otherwise returns its pid
%%
is_alive(Name) ->
  is_alive(Name, whereis(Name)).

is_alive(_, undefined) ->
  false;
is_alive(Name, Pid) ->
  is_alive(Name, Pid, process_info(Pid)).

is_alive(Name, Pid, undefined) ->
  exit(Pid, normal),
  unregister(Name),
  false;
is_alive(_, Pid, _) ->
  Pid.


-spec atom_join(List :: list()) -> atom().
%%----------------------------------------------------------------------
%% @doc Joins to atoms and returns the corresponding atom
%%----------------------------------------------------------------------
atom_join(List) ->
  atom_join(List, "_").
atom_join(List, Separator) when Separator =/= "_" ->
  Lists = atoms_to_list(List),
  string:join(Lists, Separator);
atom_join(List, Separator) ->
  Lists = atoms_to_list(List),
  list_to_atom(string:join(Lists, Separator)).

atoms_to_list(List) ->
  lists:map(
    fun(Atom) ->
      need_list(Atom)
    end, List).

-spec need_list(Match :: any()) -> list().
%%----------------------------------------------------------------------
%% @doc converts anything to list
%%----------------------------------------------------------------------
need_list(Match) when is_list(Match) ->
  Match;
need_list(Match) when is_integer(Match) ->
  integer_to_list(Match);
need_list(Match) when is_atom(Match) ->
  atom_to_list(Match);
need_list(Match) when is_binary(Match) ->
  binary_to_list(Match);
need_list(Match) when is_float(Match) ->
  float_to_list(Match);
need_list(Match) when is_tuple(Match) ->
  tuple_to_list(Match).
