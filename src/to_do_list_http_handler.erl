%%%-------------------------------------------------------------------
%%% @author joe
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2018 20:03
%%%-------------------------------------------------------------------
-module(to_do_list_http_handler).
-author("joe").

-define(HOME, file:read_file("sites/home.html")).

%% API
-export([init/3,
  handle/2,
  terminate/3]).

%% internal functions
-export([handle_get/1,
  handle_post/1]).

%%====================================================================
%% API
%%====================================================================

init({tcp, http}, Req, []) ->
  {ok, Req, cowboy_rest}. %% if this connection is accepted, the http driver calls handle/2

handle(Req, State) ->
  {ok, Req3} = case cowboy_req:method(Req) of
                 {<<"GET">>, Req2} ->
                   handle_get(Req2);
                 {<<"POST">>, Req2} ->
                   handle_post(Req2)
               end,
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%%====================================================================
%% internal functions
%%====================================================================

handle_get(Req) ->
  case cowboy_req:qs(Req) of
    {<<>>, Req2} ->
      {ok, Home} = ?HOME,
      reply(Home, Req2);
    Other ->
      io:format("Other get: ~p~n", [Other]),
      {ok, Req}
  end.

handle_post(Req) ->
  io:format("body: ~p~n", [cowboy_req:body(Req)]),
  {ok, Request, Req2} = cowboy_req:body(Req),
  io:format("1"),
  Body = case match(binary_to_list(Request)) of
           [{new_title, Title}, {details, Details}, {urgent, Urgent}] ->
             io:format("2"),
             Ref = make_ref(),
             TimeDate = calendar:local_time(),
             io:format("POST: ~p~n", [{{new_title, Title}, {details, Details}, {urgent, Urgent}}]),
             %handle_add(Ref, Title, Details, Urgent, TimeDate),
             %% new page to display
             ok;
           {edit, Ref, Title, Details, Urgent} ->
             TimeDate = calendar:local_time(),
             %handle_edit(Ref, Title, Details, Urgent, TimeDate),
             %% new page to display
             ok;
           {delete, Ref} ->
             %handle_delete(Ref),
             %% new page to display
           ok;
           Other ->
             io:format("NOT MATCHED: ~p~n", [Other])
         end,
  reply(Body, Req).

match("new_title="++Rest) ->
  match_add(Rest, "", [{new_title, undefined}, {details, undefined}, {urgent, false}]).

match_add("", Acc, Out) ->
  case {lists:keyfind(details, 1, Out), lists:reverse(Acc)} of
    {{details, undefined}, Details} ->
      lists:keystore(details, 1, Out, {details, Details});
    {{details, Details}, "on"} ->
      lists:keystore(urgent, 1, Out, {urgent, true})
  end;
match_add("&urgent="++Rest, Acc, Out) ->
  Details = lists:reverse(Acc),
  NewOut = lists:keystore(details, 1, Out, {details, Details}),
  match_add(Rest, "", NewOut);
match_add("&details="++Rest, Acc, Out) ->
  NewTitle = lists:reverse(Acc),
  NewOut = lists:keystore(new_title, 1, Out, {new_title, NewTitle}),
  match_add(Rest, "", NewOut);
match_add([H|T], Acc, Out) ->
  match_add(T, [H|Acc], Out).


reply(Body, Req) ->
  io:format("replying ~n"),
  {ok, Req2} = cowboy_req:reply(200, [], binary_to_list(Body), Req),
  io:format("replied ~n"),
  {ok, Req2}.
