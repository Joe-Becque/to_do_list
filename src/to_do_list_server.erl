%%%-------------------------------------------------------------------
%%% @author joe
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Nov 2018 19:58
%%%-------------------------------------------------------------------
-module(to_do_list_server).
-author("joe").

-behaviour(gen_server).

%% API
-export([start_link/0,
  stop/0,
  add/5,
  delete/1,
  edit/5,
  delete_all/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {to_do}). %% ets table {ref, [Title, Details, Urgency, TimeDateSet]}

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?MODULE).

add(Ref, Title, Details, Urgency, TimeDate) ->
  gen_server:call(?MODULE, {add, Ref, Title, Details, Urgency, TimeDate}).

edit(Ref, Title, Details, Urgency, TimeDate) ->
  gen_server:call(?MODULE, {edit, Ref, Title, Details, Urgency, TimeDate}).

delete(Ref) ->
  gen_server:call(?MODULE, {delete, Ref}).

delete_all() ->
  gen_server:call(?MODULE, {delete_all}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{to_do = ets:new(table, [])}}.

handle_call({add, Ref, Title, Details, Urgency, TimeDate}, _From, State = #state{to_do = ToDo}) ->
  Reply = case ets:lookup(ToDo, Ref) of
            [] ->
              ets:insert(ToDo, {Ref, {Title, Details, Urgency, TimeDate}}),
              ok;
            [{Ref, _Info}] ->
              ref_already_exist
          end,
  {reply, {Reply, ToDo}, State};
handle_call({edit, Ref, Title, Details, Urgency, TimeDate}, _From, State = #state{to_do = ToDo}) ->
  Reply = case ets:lookup(ToDo, Ref) of
            [] ->
              no_ref;
            [{Ref, _Info}] ->
              ets:insert(ToDo, {Ref, {Title, Details, Urgency, TimeDate}}),
              ok
          end,
  {reply, {Reply, ToDo}, State};
handle_call({delete, Ref}, _From, State = #state{to_do = ToDo}) ->
  ets:delete(ToDo, Ref),
  {reply, {ok, ToDo}, State};
handle_call({delete_all}, _From, State = #state{to_do = ToDo}) ->
  ets:delete(ToDo),
  NewToDo = ets:new(table, []),
  #state{to_do = NewToDo},
  {reply, {ok, NewToDo}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================






















