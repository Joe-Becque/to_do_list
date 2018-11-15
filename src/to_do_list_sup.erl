-module(to_do_list_sup).

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

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 4,
  MaxSecondsBetweenRestarts = 60,

  SupFlags = #{strategy => RestartStrategy, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts},

  ToDoListServer = #{id => to_do_list_server,
                      start => {to_do_list_server, start_link, []},
                    restart => permanent,
                   shutdown => 2000,
                       type => worker,
                    modules => [to_do_list_server]},

  {ok, {SupFlags, [ToDoListServer]}}.
