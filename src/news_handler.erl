-module(news_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([allow_missing_post/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([handle_json/2, handle_get/2]).

-include("news.hrl").

%-record(news,{title = undefined, content = undefined, id}).

%init(Req, Opts) ->
%	io:format("Init handler ~p~n", [self()]),
%	case cowboy_req:method(Req) of
%		{<<"GET">>, Req1} ->
%			handle_get(Req1);
%		{_, _} ->
%			{cowboy_rest, Req, Opts}.
%	end.

init(_Transport, _Req, _Opts) ->
	    io:format("Init: ~p~n", [self()]),
		    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.


allow_missing_post(Req, Opts) ->
	{true, Req, Opts}.

content_types_accepted(Req, State) ->
	io:format("Accepted: ~p~n", [Req]),
	{[
		{<<"application/x-www-form-urlencoded">>, handle_json},
		{<<"application/json">>, handle_json}
	], Req, State}.

content_types_provided(Req, State) ->
	io:format("Provided: ~p~n", [Req]),
	{[
		{<<"application/json">>, handle_get}
	], Req, State}.

handle_json(Req, State) ->
	io:format("Json\n"),
	{ok, Body, Req1} = cowboy_req:body(Req),
	case jsx:is_json(Body) of
		true ->
			Val = jsx:decode(Body),
			io:format("Get body: ~p~n~p~n", [Body,Val]),
			Insert = insert(Val, #news{}),
			{ok, Req2} = cowboy_req:reply(200, [], Insert, Req1),
			{true, Req2, State};
		false ->
			{ok, Req2} =
			 cowboy_req:reply(400, [], 
				<<"{\"bad_json\"}">>, Req1),
			{halt, Req2, State}
	end.

handle_get(Req, State) ->
	{News, Req1} = cowboy_req:binding(news_id, Req),
	io:format("News: ~p~n", [News]),
	{ok, Req2} =cowboy_req:reply(404, [], <<"{\"", News/binary, " not Found\"}">>,  Req1),
	{true, Req2, State}.

insert([], #news{content = undefined}) ->
	io:format("missed content\n"),
	<<"{\"field_content_not_presents\"}">>;
insert([], #news{title = undefined}) ->
	io:format("missed title\n"),
	<<"{\"field_title_not_present\"}">>;
insert([], Acc) ->
	io:format("insert all row\n"),
	Q = fun() ->
		mnesia:all_keys(news)
	end,
	{atomic, Keys} = mnesia:transaction(Q),
	Next = get_next_id(Keys),
	Acc1 = 	Acc#news{id = Next},
	F = fun() ->
		mnesia:write(Acc1)
	end,
	mnesia:transaction(F),
	BinNext = list_to_binary(integer_to_list(Next)),
	<<"{\"id\":\"", BinNext/binary, "\"}">>;
insert([{<<"content">>, Content}|T], Acc) ->
	io:format("insert content\n"),
	Acc1 = Acc#news{content = Content},
	insert(T, Acc1);
insert([{<<"title">>, Title}|T], Acc) ->
	io:format("insert title\n"),
	Acc1 = Acc#news{title = Title},
	insert(T, Acc1);
insert(Val, Acc) ->
	io:format("Bad match: ~p~n~p~n", [Val,Acc]),
	<<"{\"bad_match\"}">>.

get_next_id([]) -> 1;
get_next_id(Keys) ->
	lists:max(Keys) + 1.
