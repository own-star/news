-module(news_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	RiakHandler = {news_riak, {news_riak, start_link, []},
		permanent, 2000, worker, [news_riak]},

	Procs = [RiakHandler],
	{ok, {{one_for_one, 1, 5}, Procs}}.
