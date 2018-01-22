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

%-include_lib("stdlib/include/qlc.hrl").
-include("news.hrl").


init(_Transport, _Req, _Opts) ->
		    {upgrade, protocol, cowboy_rest}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% REST CallBacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

service_available(Req, State) ->
	case gen_server:call(news_riak, ping) of
		pong -> {true, Req, State};
		_ ->
		   Req1 = cowboy_req:set_resp_body(<<"{\"error\":\"database_error\"}">>, Req),	
			{false, Req1, State}
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
			case is_exists(NewsId) of
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
	Resp = gen_server:call(news_riak, {delete,NewsId}),
	Req2 = cowboy_req:set_resp_body(jsx:encode(Resp), Req1),
	{true, Req2, State}.

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
			NewsId1 = case NewsId of
				undefined when Method =:= <<"PUT">> ->
					{halt, Req3};
				undefined -> NewsId;
				_ when Method =:= <<"POST">> ->
					Req5 = cowboy_req:set_resp_body(
						<<"{\"error\":\"bad_request\"}">>, Req3),
					{halt, Req5};
				_ -> NewsId
			end,
			case NewsId1 of
				{halt, Req6} ->
					{false, Req6, State};
				_ ->
					Insert = handle_data(Val, Method, NewsId1),
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
			gen_server:call(news_riak, get_list);
		_ ->
			gen_server:call(news_riak, {get, NewsId})
	end,
	Json = jsx:encode(List),
	{ok, Req2} =cowboy_req:reply(200, [], Json,  Req1),
	{halt, Req2, State}.


is_exists(NewsId) ->
	gen_server:call(news_riak, {is_exists, NewsId}).


%%%%%%%%%%%%%%%%%%%  Privat Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_data(Val, <<"POST">>, undefined) ->
	try
		Content = proplists:get_value(<<"content">>, Val),
		xmerl_scan:string(binary_to_list(Content)),
		Title = proplists:get_value(<<"title">>, Val),
%		xmerl_scan:string(binary_to_list(Content), [{validation, dtd}]),
		Date = get_time(calendar:universal_time()),
		Res =   gen_server:call(news_riak, {insert, Title, Content, Date}),
		jsx:encode(Res)
	catch Err ->
		io:format("Parse error: ~p~n", [Err]),
		<<"{\"error\":\"not_valid_page\"}">>
	end;

handle_data(Val, <<"PUT">>, NewsId) ->
	update(Val, NewsId).

update(Val, NewsId) ->
	try
		Content = proplists:get_value(<<"content">>, Val),
		xmerl_scan:string(binary_to_list(Content)),
		Title = proplists:get_value(<<"title">>, Val),
		Date = get_time(calendar:universal_time()),
		Res = gen_server:call(news_riak, {update, NewsId, Title, Content, Date}),
		jsx:encode(Res)
	catch Err ->
		io:format("Parse error: ~p~n", [Err]),
		<<"{\"error\":\"not_valid_page\"}">>
	end.

get_time({{Year, Month, Day}, {Hour, Min, Sec}}) ->
	BinYear = list_to_binary(integer_to_list(Year)),
	BinMonth = list_to_binary(integer_to_list(Month)),
	BinDay = list_to_binary(integer_to_list(Day)),
	BinHour = list_to_binary(integer_to_list(Hour)),
	BinMin = list_to_binary(integer_to_list(Min)),
	BinSec = list_to_binary(integer_to_list(Sec)),
	<<BinYear/binary, "/", BinMonth/binary, "/", BinDay/binary, " ",
	BinHour/binary, ":", BinMin/binary, ":", BinSec/binary, " UTC">>.

