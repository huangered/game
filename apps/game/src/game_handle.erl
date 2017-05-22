-module(game_handle).

-export([query/1]).

query(ActionId) ->
	io:format("Query action fun ~p~n", [ActionId]).