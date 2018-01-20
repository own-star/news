-module(news_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	mnesia:start(),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/news/[:news_id]", news_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8008}], [ 
			{env, [{dispatch, Dispatch}]}
	]),
	news_sup:start_link().

stop(_State) ->
	ok.
