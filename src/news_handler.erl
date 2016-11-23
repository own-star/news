-module(news_handler).

-export([init/3]).

%% REST Callbacks
-export([allowed_methods/2]).
-export([allow_missing_post/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([delete_resource/2, delete_completed/2]).

%% Handlers
-export([handle_json/2, handle_get/2]).

-include_lib("stdlib/include/qlc.hrl").
-include("news.hrl").


init(_Transport, _Req, _Opts) ->
		    {upgrade, protocol, cowboy_rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% REST CallBacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.


allow_missing_post(Req, Opts) ->
	{true, Req, Opts}.

content_types_accepted(Req, State) ->
	{[
		{<<"application/x-www-form-urlencoded">>, handle_json},
		{<<"application/json">>, handle_json}
	], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"*/*">>, handle_get}
	], Req, State}.

delete_resource(Req, State) ->
	{NewsId, Req1} = cowboy_req:binding(news_id, Req),
	Result =case NewsId of
		undefined -> <<"{\"news_not_found\"}">>;
		_ -> delete(binary_to_integer(NewsId))
	end,
	{Result, Req1, State}.

delete_completed(Req, State) ->
	{ok, Req1} = cowboy_req:reply(200, [], <<"{\"deleted\"}">>, Req),
	{true, Req1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%% CallBack Handlers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_json(Req, State) ->
	{ok, Body, Req1} = cowboy_req:body(Req),
	case jsx:is_json(Body) of
		true ->
			Val = jsx:decode(Body),
			{Method, Req2} = cowboy_req:method(Req1),
			{NewsId, Req3} = cowboy_req:binding(news_id, Req2),
			News = case NewsId of
				undefined -> NewsId;
				_ -> binary_to_integer(NewsId)
			end,
			Insert = handle_data(Val, Method, News),
			{ok, Req4} = cowboy_req:reply(200, [], Insert, Req3),
			{true, Req4, State};
		false ->
			{ok, Req2} =
			 cowboy_req:reply(400, [], 
				<<"{\"bad_json\"}">>, Req1),
			{halt, Req2, State}
	end.

handle_get(Req, State) ->
	{NewsId, Req1} = cowboy_req:binding(news_id, Req),
	List = case NewsId of
		undefined ->
			do(qlc:q([{X#news.id, X#news.udate, X#news.title} || 
					X <- mnesia:table(news)]));
		_ -> do(qlc:q([X || 
					X <- mnesia:table(news), X#news.id =:= binary_to_integer(NewsId)]))
	end,
	Json = get_json(List, []),
	{ok, Req2} =cowboy_req:reply(200, [], Json,  Req1),
	{true, Req2, State}.



%%%%%%%%%%%%%%%%%%%  Privat Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_data(Val, <<"PUT">>, NewsId) ->
	update(Val, NewsId);
handle_data(Val, <<"POST">>, _NewsId) ->
	insert(Val, #news{}).

delete(NewsId) ->
	F = fun() ->
		mnesia:delete({news, NewsId})
	end,
	{Res, _} = mnesia:transaction(F),
	case Res of
		atomic -> true;
		aborted -> false
	end.

update(Val, NewsId) when is_integer(NewsId) ->
		[News] = do(qlc:q([X || X <- mnesia:table(news),
								X#news.id =:= NewsId])),
	update(Val, News);
update([{<<"title">>, Title} | Rest], #news{} = News0) ->
	News = News0#news{title = Title},
	update(Rest, News);
update([{<<"content">>, Content} | Rest], #news{} = News0) ->
	News = News0#news{content = Content},
	update(Rest, News);
update([], #news{} = News0) ->
	Date = get_time(calendar:universal_time()),
	News = News0#news{udate = Date},
	F = fun() ->
		mnesia:write(News)
	end,
	{Res, _} = mnesia:transaction(F),
	case Res of 
		aborted -> <<"{\"error\":\"not_updated\"}">>;
		atomic -> <<"{\"success\":\"updated\"}">>
	end.

insert([], #news{content = undefined}) ->
	<<"{\"error\":\"field_content_not_presents\"}">>;
insert([], #news{title = undefined}) ->
	<<"{\"error\":\"field_title_not_present\"}">>;
insert([], Acc) ->
	Q = fun() ->
		mnesia:all_keys(news)
	end,
	{atomic, Keys} = mnesia:transaction(Q),
	Next = get_next_id(Keys),
	Date = get_time(calendar:universal_time()),
	Acc1 = 	Acc#news{id = Next, cdate = Date, udate = Date},
	F = fun() ->
		mnesia:write(Acc1)
	end,
	{Res , _} = mnesia:transaction(F),
	case Res of
		atomic ->
			BinNext = list_to_binary(integer_to_list(Next)),
			<<"{\"id\":\"", BinNext/binary, "\",\"create_time\":\"", Date/binary, "\"}">>;
		aborted ->
			<<"{\"error\":\"not_added\"}">>
	end;
insert([{<<"content">>, Content}|T], Acc) ->
	Acc1 = Acc#news{content = Content},
	insert(T, Acc1);
insert([{<<"title">>, Title}|T], Acc) ->
	Acc1 = Acc#news{title = Title},
	insert(T, Acc1);
insert(_Val, _Acc) ->
	<<"{\"error\":\"bad_match\"}">>.

get_next_id([]) -> 1;
get_next_id(Keys) ->
	lists:max(Keys) + 1.

get_time({{Year, Month, Day}, {Hour, Min, Sec}}) ->
	BinYear = list_to_binary(integer_to_list(Year)),
	BinMonth = list_to_binary(integer_to_list(Month)),
	BinDay = list_to_binary(integer_to_list(Day)),
	BinHour = list_to_binary(integer_to_list(Hour)),
	BinMin = list_to_binary(integer_to_list(Min)),
	BinSec = list_to_binary(integer_to_list(Sec)),
	<<BinYear/binary, "/", BinMonth/binary, "/", BinDay/binary, " ",
	BinHour/binary, ":", BinMin/binary, ":", BinSec/binary, " UTC">>.

get_json([], Acc) ->
	jsx:encode(Acc);
get_json([{Id, UpdateDate, Title} | Rest], Acc) ->
	BinId = list_to_binary(integer_to_list(Id)),
	get_json(Rest, [[{<<"id">>, BinId}, {<<"update_time">>, UpdateDate}, {<<"title">>, Title}] | Acc]);
get_json([{_, Id, Title, Content, CreateDate, UpdateDate}], _Acc) ->
	BinId = list_to_binary(integer_to_list(Id)),
	jsx:encode([{<<"id">>, BinId}, {<<"create_time">>, CreateDate}, {<<"update_time">>, UpdateDate}, {<<"title">>, Title}, {<<"content">>, Content}]).

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.
