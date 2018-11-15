-module(to_do_list_app).

-behaviour(application).

%% API
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  N_acceptors = 10,
  Port = application:get_env(to_do_list, httpport, 8080),
  Dispatch = cowboy_router:compile([
    {'_',
      [{"/[:request]", to_do_list_http_handler, []}]
    }]),
  cowboy:start_http(http,
    N_acceptors,
    [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  to_do_list_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

