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
-record(ref, {references}).
%% API
-export([init/3,
  handle/2,
  terminate/3]).

%% internal functions
-export([handle_get/1,
  handle_post/1,
  to_string/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% more than one word in the title and

%% timedate formatting

%% urgent - more user friendly


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
      io:format("GET HOME: ~n"),
      {ok, Home} = ?HOME,
      reply(Home, Req2);
    Other ->
      io:format("Other get: ~p~n", [Other]),
      io:format("body: ~p~n", [cowboy_req:body(Req)]),
      {ok, Req}
  end.

handle_post(Req) ->
  {ok, Request, Req2} = cowboy_req:body(Req),
  io:format("1 ~n"),
  io:format("Request: ~p~n", [Request]),
  Body = case match(binary_to_list(Request)) of
           [{new_title, Title}, {details, Details}, {urgent, Urgent}] ->
             Ref = to_do_list_server:new_ref(),
             TimeDate = calendar:local_time(),
             io:format("new stuff: ~p~n", [{Ref, Title, Details, Urgent, TimeDate}]),
             {ok, ToDoList} =  to_do_list_server:add(Ref, Title, Details, Urgent, TimeDate),
             io:format("POST: ~p~n", [ToDoList]),
             html_state(ToDoList);
           [{edit_title, Title}, {edit_details, Details}, {edit_urgent, Urgent}, {reference, Ref}] ->
             TimeDate = calendar:local_time(),
             io:format("edit stuff: ~p~n", [{Ref, Title, Details, Urgent, TimeDate}]),
             {ok, ToDoList} = to_do_list_server:edit(Ref, Title, Details, Urgent, TimeDate),
             %% new page to display
             html_state(ToDoList);
           {delete, Ref} ->
             {ok, ToDoList} = to_do_list_server:delete(Ref),
             io:format("DELETE: ~p~n", [Ref]),
             io:format("ToDoList: ~p~n", [ToDoList]),
             html_state(ToDoList);
           Other ->
             io:format("NOT MATCHED: ~p~n", [Other])
         end,
  reply(Body, Req2).

match("delete" ++ Rest) ->
  match_delete(Rest);
match("edit_title=" ++ Rest) ->
  io:format("EDITING:  ~p~n", [Rest]),
  match_edit(Rest, "", [{edit_title, undefined}, {edit_details, undefined}, {edit_urgent, false}, {reference, undefined}]);
match("new_title=" ++ Rest) ->
  io:format("match, REST: ~p~n", [Rest]),
  match_add(Rest, "", [{new_title, undefined}, {details, undefined}, {urgent, false}]);
match(Ok) ->
  io:format("MATCH DID NOT MATCH ~p~n", [Ok]).

match_delete(Rest) -> match_delete(Rest, "").
match_delete("=Delete", Acc) ->
  Ref = list_to_integer(lists:reverse(Acc)),
  io:format("Delete Ref: ~p~n", [Ref]),
  {delete, Ref};
match_delete([H|T], Acc) -> match_delete(T, [H|Acc]).

match_edit("=edit", Acc, Out) ->
  io:format("reference ~n"),
  io:format("edit_details: ~p~n", [lists:keyfind(edit_details, 1, Out)]),
  io:format("Acc: ~p~n", [lists:reverse(Acc)]),
  case {lists:keyfind(edit_details, 1, Out), lists:reverse(Acc)} of
    {{edit_details, undefined}, Details} ->
      lists:keystore(edit_details, 1, Out, {edit_details, Details});
    {{edit_details, Details}, Ref} ->
      lists:keystore(reference, 1, Out, {reference, list_to_integer(Ref)})
  end;
match_edit("&reference"++Rest, Acc, Out) ->
  NewDetails = lists:reverse(Acc),
  io:format("New Details or urgent: ~p~n", [NewDetails]),
  io:format(" {lists:keyfind(edit_details, 1, Out), lists:reverse(Acc)}: ~p~n", [ {lists:keyfind(edit_details, 1, Out), lists:reverse(Acc)}]),
  NewOut = case {lists:keyfind(edit_details, 1, Out), lists:reverse(Acc)} of
    {{edit_details, undefined}, Details} ->
      lists:keystore(edit_details, 1, Out, {edit_details, Details});
    {{edit_details, Details}, "on"} ->
      lists:keystore(edit_urgent, 1, Out, {edit_urgent, true})
           end,
  match_edit(Rest, "", NewOut);
match_edit("&urgent="++Rest, Acc, Out) ->
  NewDetails = lists:reverse(Acc),
  io:format("NewUrgent: ~p~n", [NewDetails]),
  NewOut = lists:keystore(edit_details, 1, Out, {edit_details, NewDetails}),
  match_edit(Rest, "", NewOut);
match_edit("&edit_details="++Rest, Acc, Out) ->
  NewTitle = lists:reverse(Acc),
  io:format("NewTitle: ~p~n", [NewTitle]),
  NewOut = lists:keystore(edit_title, 1, Out, {edit_title, NewTitle}),
  match_edit(Rest, "", NewOut);
match_edit([H|T], Acc, Out) ->
  match_edit(T, [H|Acc], Out).

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

%% returns html code for the body of the page as a string
html_state(ToDoList) -> html_state(ToDoList, "", "", "").

html_state([], AccTop, AccMiddle, AccBottom) ->
  make_page(AccTop, AccMiddle, AccBottom);
html_state([{Ref, {Title, Details, Urgent, TimeDate}} | T], AccTop, AccMiddle, AccBottom) ->
  io:format("{Ref, {Title, Details, Urgent, TimeDate}}: ~p~n", [{Ref, {Title, Details, Urgent, TimeDate}}]),
  %html code with one to_do item
  {Top, Middle, Bottom} = html_item(Ref, Title, Details, Urgent, TimeDate),
  html_state(T, [Top|AccTop], [Middle|AccMiddle], [Bottom|AccBottom]).

reply(Body, Req) when is_binary(Body)->
  reply(binary_to_list(Body), Req);
reply(Body, Req) ->
  io:format("replying: ~n"),
  {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
  io:format("replied ~n"),
  {ok, Req2}.

html_item(Ref, Title, Details, Urgent, TimeDate) ->
  io:format("Title: ~p~n", [Title]),
  Top = "    /* The Modal (background) */
    .modal"++to_string(Ref)++" {
    display: none; /* Hidden by default */
    position: fixed; /* Stay in place */
    z-index: 1; /* Sit on top */
    padding-top: 100px; /* Location of the box */
    left: 0;
    top: 0;
    width: 100%; /* Full width */
    height: 100%; /* Full height */
    overflow: auto; /* Enable scroll if needed */
    background-color: rgb(0,0,0); /* Fallback color */
    background-color: rgba(0,0,0,0.4); /* Black w/ opacity */
}

/* Modal Content */
.modal-content"++to_string(Ref)++" {
    background-color: #fefefe;
    margin: auto;
    padding: 20px;
    border: 1px solid #888;
    width: 80%;
}

/* The Close Button */
.close"++to_string(Ref)++" {
    color: #aaaaaa;
    float: right;
    font-size: 28px;
    font-weight: bold;
}

.close"++to_string(Ref)++":hover,
.close"++to_string(Ref)++":focus {
    color: #000;
    text-decoration: none;
    cursor: pointer;
}",
  Middle =   "<tr>
    <td>"++Title++"</td>
    <td>"++Details++"</td>
    <td>"++to_string(TimeDate)++"</td>
    <td>"++to_string(Urgent)++"</td>
    <td>
    <form action=\"\" method=\"post\">
    <input type=\"submit\" name=\"delete"++to_string(Ref)++"\" value=\"Delete\">
    </form>
    </td>

    <td>
    <!-- Trigger/Open The Modal -->
    <button id=\"myBtn"++to_string(Ref)++"\">Edit</button>

    <!-- The Modal -->
    <div id=\"myModal"++to_string(Ref)++"\" class=\"modal"++to_string(Ref)++"\">

    <!-- Modal content -->
    <div class=\"modal-content"++to_string(Ref)++"\">
    <span class=\"close"++to_string(Ref)++"\">&times;</span>
    <form action=\"\" method=\"post\">
        <fieldset>
            Edit Title:<br>
            <input type=\"text\" name=\"edit_title\" value=\"\">
            <br><br>
            Edit Details:<br>
            <input type=\"text\" name=\"edit_details\" value=\""++to_string(Details)++"\">
            Urgent?:
            <input type=\"checkbox\" name=\"urgent\">
            <br><br>
            <input type=\"submit\" name=\"reference"++to_string(Ref)++"\" value=\"edit\">
        </fieldset>
    </form>
    </div>
    </div>
    </td>
  </tr>
  ",
  Bottom = "// Get the modal
var modal"++to_string(Ref)++" = document.getElementById(\"myModal"++to_string(Ref)++"\");

// Get the button that opens the modal
var btn"++to_string(Ref)++" = document.getElementById(\"myBtn"++to_string(Ref)++"\");

// Get the <span> element that closes the modal
var span"++to_string(Ref)++" = document.getElementsByClassName(\"close"++to_string(Ref)++"\")[0];

// When the user clicks the button, open the modal
btn"++to_string(Ref)++".onclick = function() {
    modal"++to_string(Ref)++".style.display = \"block\";
}

// When the user clicks on <span> (x), close the modal
span"++to_string(Ref)++".onclick = function() {
    modal"++to_string(Ref)++".style.display = \"none\";
}
  ",
  {Top, Middle, Bottom}.

make_page(Top, Middle, Bottom) ->
  "<!DOCTYPE html>
    <html>
    <body>

    <style>
    table, th, td {
    text-align: left;
    color: black;
    background-color: gray;
    }
    "++Top++"
    </style>

    <h2>To Do List</h2>

    <br> <br>

    <form action=\"\" method=\"post\">
        <fieldset>
            <legend>Add Item:</legend>
            Title:<br>
            <input type=\"text\" name=\"new_title\">
            <br><br>
            Details:<br>
            <input type=\"text\" name=\"details\">
            Urgent?:
            <input type=\"checkbox\" name=\"urgent\">
            <br><br>
            <input type=\"submit\" value=\"OK\">
        </fieldset>
    </form>

    <br><br>

    <table style=\"width:100%\">
    <tr>
       <th>Title</th>
       <th>Details</th>
       <th>Time Set</th>
       <th>Urgent?</th>
    </tr>
    "++Middle++"
  </table>
  <script>

"++Bottom++"

</script>

  </body>
  </html>".

to_string(In) ->
  R = io_lib:format("~p",[In]),
  lists:flatten(R).