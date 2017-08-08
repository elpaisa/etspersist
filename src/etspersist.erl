%%%-------------------------------------------------------------------
%%% @author johnleytondiaz
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2017 1:43 PM
%%%-------------------------------------------------------------------
-module(etspersist).
-author("johnleytondiaz").

%% API
-export([new/1, get/2, get/3, take/2, take/3, insert/2, insert/3, update/3]).


-spec new(ETS :: atom()) -> tuple().
%%
%% @doc Creates an ets table by name
%% @param ETS name of the table to create
%%
new(ETS) ->
  new(ETS, ets:info(ETS)).

new(ETS, undefined)->
  etspersist_srv:new_ets(ETS);
  %%ets:new(ETS, [set, named_table, public, compressed]);
new(_, Table)->
  Table.


-spec get(ETS :: atom(), Key :: any()) ->  {ok, Table :: tuple(), any()}.
%%
%% @doc Gets an item from an ets
%% @equiv get(Table, ETS, Key)
%% @param ETS name of the table to get the key from
%% @param Key key name to get from the table
%%
get(ETS, Key) ->
  get(new(ETS), ETS, Key).

-spec get(Table :: tuple(), ETS :: atom(), Key :: any()) ->  {ok, Table :: tuple(), any()}.
%%
%% @doc Gets an item from an ets
%% @param Table actual ETS table
%% @param ETS name of the table to get the key from
%% @param Key key name to get from the table
%%
get(Table, ETS, Key) ->
  Search = ets:lookup(ETS, Key),
  {ok, Table, Search}.

-spec take(ETS :: atom(), Key :: any()) ->  {ok, Table :: tuple(), any()}.
%%
%% @doc Takes an item from an ets, that means the key will be deleted as soon as it
%% is retrieved
%% @equiv take(Table, ETS, Key)
%% @param ETS name of the table to get the key from
%% @param Key key name to get from the table
%%
take(ETS, Key) ->
  take(new(ETS), ETS, Key).

-spec take(Table :: tuple(), ETS :: atom(), Key :: any()) ->  {ok, Table :: tuple(), any()}.
%%
%% @doc Takes an item from an ets
%% @param Table actual ETS table
%% @param ETS name of the table to take the key from
%% @param Key key name to get from the table
%%
take(Table, ETS, Key) ->
  Take = ets:take(ETS, Key),
  {ok, Table, Take}.


-spec insert(ETS :: atom(), Key :: any()) ->  {ok, Table :: tuple(), any()}.
%%
%% @doc Inserts an item into an ets
%% @equiv insert(Table, ETS, Value)
%%
insert(ETS, Value) ->
  insert(new(ETS), ETS, Value).

-spec insert(Table :: tuple(), ETS :: atom(), Key :: any()) ->  {ok, Table :: tuple(), any()}.
%%
%% @doc Inserts an item into an ets
%% @param Table actual ETS table
%% @param ETS name of the table to insert the record
%% @param Value actual record to be inserted
%%
insert(Table, ETS, Value) ->
  Insert = ets:insert(ETS, Value),
  {ok, Table, Insert}.

-spec update(ETS :: atom(), Position :: integer(), Value :: any()) ->  {ok, Table :: tuple(), any()}.
%%
%% @doc Updates an item in an ets
%% @equiv update(Table, ETS, Position, Value)
%%
update(ETS, Position, Value) ->
  update(new(ETS), ETS, Position, Value).

-spec update(Table :: tuple(), ETS :: atom(), Position :: integer(), Value :: any()
) ->  {ok, Table :: tuple(), any()}.
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
