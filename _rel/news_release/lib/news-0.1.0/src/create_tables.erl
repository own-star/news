-module(create_tables).

-include("news.hrl").

-export([init/0]).

init() ->
	mnesia:stop(),
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(news, [{disc_copies, [node()]}, {attributes, record_info(fields, news)}]).
