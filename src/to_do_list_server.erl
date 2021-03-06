-module(to_do_list_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0,
         add/5,
         delete/1,
         new_ref/0,
         edit/5,
         delete_all/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% handle functions
-export([handle_add/6,
         handle_edit/6,
         handle_delete/2,
         handle_new_ref/1,
         handle_delete_all/1]).

-define(SERVER, ?MODULE).

-record(state, {to_do,        %% ets table {ref, [Title, Details, Urgency, TimeDateSet]}
                references}). %% list [0,1,2,..]

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

new_ref() ->
  gen_server:call(?MODULE, {new_ref}).

delete_all() ->
  gen_server:call(?MODULE, {delete_all}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{to_do = ets:new(table, []), references = [0]}}.

handle_call({add, Ref, Title, Details, Urgency, TimeDate}, _From, State) ->
  handle_add(Ref, Title, Details, Urgency, TimeDate, State);
handle_call({edit, Ref, Title, Details, Urgency, TimeDate}, _From, State) ->
  handle_edit(Ref, Title, Details, Urgency, TimeDate, State);
handle_call({delete, Ref}, _From, State) ->
  handle_delete(Ref, State);
handle_call({new_ref}, _From, State) ->
  handle_new_ref(State);
handle_call({delete_all}, _From, State) ->
  handle_delete_all(State).

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% handle functions
%%%===================================================================

handle_add(Ref, Title, Details, Urgency, TimeDate, State = #state{to_do = ToDo, references = _Refs}) ->
  ets:insert(ToDo, {Ref, {Title, Details, Urgency, TimeDate}}),
  ToDoList = ets:match_object(ToDo, {'$0', '$1'}),
  {reply, {ok, ToDoList}, State}.

handle_edit(Ref, Title, Details, Urgency, TimeDate, State = #state{to_do = ToDo}) ->
  case ets:lookup(ToDo, Ref) of
    [] ->
      no_ref;
    [{Ref, _Info}] ->
      ets:insert(ToDo, {Ref, {Title, Details, Urgency, TimeDate}}),
      ok
  end,
  ToDoList = ets:match_object(ToDo, {'$0', '$1'}),
  {reply, {ok, ToDoList}, State}.

handle_delete(Ref, State = #state{to_do = ToDo}) ->
  ets:delete(ToDo, Ref),
  ToDoList = ets:match_object(ToDo, {'$0', '$1'}),
  {reply, {ok, ToDoList}, State}.

handle_new_ref(_State = #state{to_do = ToDo, references = Refs}) ->
  NewRef = lists:max(Refs)+1,
  NewRefs = lists:append(Refs, [NewRef]),
  NewState = #state{to_do = ToDo, references = NewRefs},
  {reply, NewRef, NewState}.

handle_delete_all(State = #state{to_do = ToDo}) ->
  ets:delete(ToDo),
  NewToDo = ets:new(table, []),
  #state{to_do = NewToDo},
  {reply, {ok, NewToDo}, State}.