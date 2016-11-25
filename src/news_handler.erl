-module(news_handler).

-export([init/3]).

%% REST Callbacks
-export([service_available/2]).
-export([allowed_methods/2]).
-export([allow_missing_post/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([previously_existed/2]).
-export([delete_resource/2, delete_completed/2, is_exists/1]).

%% Handlers
-export([handle_json/2, handle_get/2]).

-include_lib("stdlib/include/qlc.hrl").
-include("news.hrl").


init(_Transport, _Req, _Opts) ->
		    {upgrade, protocol, cowboy_rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% REST CallBacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

service_available(Req, State) ->
	F = fun() ->
			mnesia:all_keys(news)
	end,
	case mnesia:transaction(F) of
		{aborted, _} ->
			Req1 = cowboy_req:set_resp_body(<<"{\"error\":\"databese_error\"}">>, Req),
			{false, Req1, State};
		{atomic, _} -> {true, Req, State}
	end.

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
		{<<"application/json">>, handle_get}
	], Req, State}.

resource_exists(Req, State) ->
	{NewsId, Req1} = cowboy_req:binding(news_id, Req),
	{Method, Req2} = cowboy_req:method(Req1),
	case NewsId of
		undefined ->
			case Method of
				<<"DELETE">> ->
					Req3 = cowboy_req:set_resp_body(
						<<"{\"error\":\"bad_request\"}">>, Req2),
					{false, Req3, State};
				<<"PUT">> ->
					Req3 = cowboy_req:set_resp_body(
						<<"{\"error\":\"bad_request\"}">>, Req2),
					{false, Req3, State};
				_ -> {true, Req2, State}
			end;
		_ -> 
			IntNewsId = binary_to_integer(NewsId),
			case is_exists(IntNewsId) of
				true -> 
					{true, Req1, State};
				false ->
					Req4 = cowboy_req:set_resp_body(
						<<"{\"error\":\"resource_does_not_exists\"}">>, Req1),
					{false, Req4, State}
			end
	end.

previously_existed(Req, State) ->
	{false, Req, State}.

delete_resource(Req, State) ->
	{NewsId, Req1} = cowboy_req:binding(news_id, Req),
	case delete(binary_to_integer(NewsId)) of
		false -> 
			Req2 = cowboy_req:set_resp_body( 
				<<"{\"error\":\"has_not_deleted\"}">>, Req1),
			{false, Req2, State};
		true ->	{true, Req1, State}
	end.

delete_completed(Req, State) ->
	Req1 = cowboy_req:set_resp_body(<<"{\"success\":\"deleted\"}">>, Req),
	{true, Req1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%% CallBack Handlers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_json(Req, State) ->
	{ok, Body, Req1} = cowboy_req:body(Req),
	case jsx:is_json(Body) of
		true ->
			Val = jsx:decode(Body),
			{Method, Req2} = cowboy_req:method(Req1),
			{NewsId, Req3} = cowboy_req:binding(news_id, Req2),
			IntNewsId = case NewsId of
				undefined when Method =:= <<"PUT">> ->
					{halt, Req3};
				undefined -> NewsId;
				_ when Method =:= <<"POST">> ->
					Req5 = cowboy_req:set_resp_body(
						<<"{\"error\":\"bad_request\"}">>, Req3),
					{halt, Req5};
				_ -> binary_to_integer(NewsId)
			end,
			case IntNewsId of
				{halt, Req6} ->
					{false, Req6, State};
				_ ->
					Insert = handle_data(Val, Method, IntNewsId),
					Req4 = cowboy_req:set_resp_body(Insert, Req3),
					{true, Req4, State}
			end;
		false ->
			Req2 = cowboy_req:set_resp_body(
				<<"{\"error\":\"bad_json\"}">>, Req1),
			{false, Req2, State}
	end.

handle_get(Req, State) ->
	{NewsId, Req1} = cowboy_req:binding(news_id, Req),
	List = case NewsId of
		undefined ->
			do(qlc:q([{X#news.id, X#news.udate, X#news.title} || 
					X <- mnesia:table(news)]));
		_ ->
		   IntNewsId = binary_to_integer(NewsId),
	   		case is_exists(IntNewsId) of
				false -> [{<<"error">>, <<"does_not_exists">>}];
				true ->
					do(qlc:q([X || 
						X <- mnesia:table(news), X#news.id =:= binary_to_integer(NewsId)]))
			end
	end,
	Json = get_json(List, []),
	{ok, Req2} =cowboy_req:reply(200, [], Json,  Req1),
	{halt, Req2, State}.


is_exists(NewsId) ->
	NewsList = do(qlc:q([ X#news.id || X <- mnesia:table(news)])),
	case NewsList of
		aborted -> false;
		_ -> lists:member(NewsId, NewsList)
	end.


%%%%%%%%%%%%%%%%%%%  Privat Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_data(Val, <<"POST">>, undefined) ->
	insert(Val, #news{}, 0);
handle_data(Val, <<"PUT">>, NewsId) ->
	update(Val, NewsId).

delete(NewsId) ->
	F = fun() ->
		mnesia:delete({news, NewsId})
	end,
	Res = mnesia:transaction(F),
	case Res of
		{atomic, ok} -> true;
		_ -> false
	end.

update(Val, NewsId) when is_integer(NewsId) ->
	case is_exists(NewsId) of
		true ->
			[News] = do(qlc:q([X || X <- mnesia:table(news),
								X#news.id =:= NewsId])),
			case News of 
				aborted -> <<"{\"error\":\"database_error\"}">>;
				_ -> update(Val, News)
			end;
		false ->
			insert(Val, #news{}, NewsId)
	end;
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
	end;
update(_Val, _NewsId) ->
	<<"{\"error\":\"bad_match\"}">>.

insert([], #news{content = undefined}, _NewsId) ->
	<<"{\"error\":\"field_content_not_presents\"}">>;
insert([], #news{title = undefined}, _NewsId) ->
	<<"{\"error\":\"field_title_not_present\"}">>;
insert([], Acc, NewsId) ->
	Next = case NewsId of
		0 ->
			Q = fun() ->
				mnesia:all_keys(news)
			end,
			{KeyRes, Keys} = mnesia:transaction(Q),
			case KeyRes of
				atomic -> get_next_id(Keys);
				aborted -> <<"{\"error\":\"database_error\"}">>
			end;
		_ -> NewsId
	end,
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
insert([{<<"content">>, Content}|T], Acc, NewsId) ->
	Acc1 = Acc#news{content = Content},
	insert(T, Acc1, NewsId);
insert([{<<"title">>, Title}|T], Acc, NewsId) ->
	Acc1 = Acc#news{title = Title},
	insert(T, Acc1, NewsId);
insert(_Val, _Acc, _NewsId) ->
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
	jsx:encode([{<<"id">>, BinId}, {<<"create_time">>, CreateDate}, {<<"update_time">>, UpdateDate}, {<<"title">>, Title}, {<<"content">>, Content}]);
get_json(List, _Acc) ->
	jsx:encode(List).

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{Res, Val} = mnesia:transaction(F),
	case Res of
		aborted -> aborted;
		atomic -> Val
	end.
