-module(news_riak).

-behaviour(gen_server).

-export([start_link/0]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2
	]).

-define(SERVER, ?MODULE).

%-include_lib("riakc/include/riakc.hrl").

-record(state, {pid, id}).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
	case riakc_pb_socket:get(Pid, <<"news">>, <<"count">>) of
		{error,notfound} ->
			NewCountObj = riakc_obj:new(<<"news">>, <<"count">>, <<"0">>),
			riakc_pb_socket:put(Pid, NewCountObj),
			{ok, #state{pid = Pid, id = 0}};
		{ok, CountObj} ->
			Count = riakc_obj:get_value(CountObj),
			{ok, #state{pid = Pid, id = binary_to_integer(Count) + 1}};
		Other ->
			io:format("Unexpected get count: ~p~n", [Other]),
			{ok, #state{pid = Pid, id = undefined}}
	end.


handle_call(ping, _ref, #state{pid = Pid} = State) ->
	{reply, riakc_pb_socket:ping(Pid), State};

handle_call({is_exists, Id}, _Ref, #state{pid = Pid} = State) ->
	try riakc_pb_socket:get(Pid, <<"news">>, Id) of
		{ok, _} ->
			{reply, true, State};
		_ -> {reply, false, State}
	catch 
		_Err ->
			{reply, false, State}
	end;

handle_call({get, Id}, _Ref, #state{pid = Pid} = State) ->
	try	riakc_pb_socket:get(Pid, <<"news">>, Id) of
		{ok, Obj} ->
			Res = binary_to_term(riakc_obj:get_value(Obj)),
			{reply, Res, State};
		{error, notfound} ->
			{reply, [{<<"error">>, <<"News with ", Id/binary, " not found">>}], State}
	catch
		Err ->
			{reply, [Err], State}
	end;

handle_call(get_list, _Ref, #state{pid = Pid, id = Id} = State) ->
	{reply, get_list(Id, Pid), State};

handle_call({insert, _, _, _}, _Ref, #state{id = undefined} = State) ->
	{reply, [{<<"error">>, <<"wrong db status">>}], State};

handle_call({insert, Title, Content, Date}, _Ref, #state{pid = Pid, id = Id} = State) ->
	Obj = riakc_obj:new(<<"news">>, integer_to_binary(Id), [{<<"title">>, Title}, {<<"content">>, Content}, {<<"date">>, Date}]),
	try
		Res = riakc_pb_socket:put(Pid, Obj),
		io:format("put res: ~p~n", [Res]),
		CountObj = riakc_obj:new(<<"news">>, <<"count">>, integer_to_binary(Id)),
		riakc_pb_socket:put(Pid, CountObj),
		{reply, [{<<"success">>, Res}], State#state{id = Id + 1}}
	catch Err ->
		{reply, [{<<"error">>, list_to_binary(Err)}], State}
	end;

handle_call({update, Id, Title, Content, Date}, _Ref, #state{pid = Pid} = State) ->
	try riakc_pb_socket:get(Pid, <<"news">>, Id) of
		{ok, Obj} ->
			NewObj = riakc_obj:update_value(Obj, [{<<"title">>, Title}, {<<"content">>, Content}, {<<"date">>, Date}]),
			case riakc_pb_socket:put(Pid, NewObj) of
				{ok, _} ->
					{reply, [{<<"success">>, <<"update">>}], State};
				ok ->
					{reply, [{<<"success">>, <<"update">>}], State};
				Other ->
					{reply, [{<<"error">>, term_to_binary(Other)}], State}
			end;
		_ ->
			{reply, [{<<"error">>, <<"invalid_argument">>}],State}
	catch
		Err ->
			{reply, [{<<"error">>, list_to_binary(Err)}],State}
	end;

handle_call({delete, Id}, _Ref, #state{pid = Pid} = State) ->
	try riakc_pb_socket:delete(Pid, <<"news">>, Id) of
		ok -> 
			{reply, [{<<"success">>, <<"news ", Id/binary, " deleted">>}], State};
		Other ->
			{reply, [{<<"error">>, list_to_binary(Other)}], State}
	catch
		Err ->
			{reply, [{<<"error">>, list_to_binary(Err)}], State}
	end;


handle_call(get_pid, _Ref, State) ->
	io:format("news_riak get_pid~n"),
	{reply, self(), State};

handle_call(_, _, State) ->
	{reply, ok, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(Msg, State) ->
	io:format("news_riak get info Msg: ~p~n", [Msg]),
	{noreply, State}.

%%%%%%%%%%%% Internal functions %%%%%%%%%%%%%%

get_list(Id, Pid) ->
	get_list(lists:reverse(lists:seq(0, Id)), Pid, []).

get_list([H | T], Pid, Acc) ->
	try riakc_pb_socket:get(Pid, <<"news">>, integer_to_binary(H)) of
		{ok, Obj} ->
			[Title, _, Date] = binary_to_term(riakc_obj:get_value(Obj)),
			get_list(T, Pid, [[{<<"id">>, integer_to_binary(H)}, Title, Date] | Acc]);
%			get_list(T, Pid, [[{<<"id">>, integer_to_binary(H)}, {<<"title">>, Title}, {<<"date">>, Date}] | Acc]);
		_ ->
			get_list(T, Pid, Acc)
	catch
		_Err ->
			get_list(T, Pid, Acc)
	end;
get_list([], _Pid, Acc) ->
	io:format("List: ~p~n", [Acc]),
	Acc.
