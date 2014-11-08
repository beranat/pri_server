% PriServer meck for testing
%===============================================================================
% This module meck's the main pri_server functionality, as init/call/cast/terminate
%===============================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(PRI_SERVER_EMU_TABLE,	'pri_server_emu_server').
-define(PRI_SERVER_EMU_CLIENT,	'pri_server_emu_client').

%------------------------------------------------------------------------------
pri_server_emu_init(Args) ->
	ok = meck:new(pri_server),

	ok = meck:expect(pri_server, call, fun pri_server_emu_server_call/2),
	ok = meck:expect(pri_server, call, fun pri_server_emu_server_call/3),
	ok = meck:expect(pri_server, call, fun pri_server_emu_server_call/4),

	ok = meck:expect(pri_server, cast, fun pri_server_emu_server_cast/2),
	ok = meck:expect(pri_server, cast, fun pri_server_emu_server_cast/3),

	ok = meck:expect(pri_server, reply, fun pri_server_emu_server_reply/2),

	ets:new(?PRI_SERVER_EMU_TABLE, [named_table, set, private]),
	Result = erlang:apply(?MODULE, init, [Args]),
	?assertNot(pri_server_emu_has(reply)),
	?assertMatch({ok, _State}, Result),
	{ok, State} = Result,
	pri_server_emu_set(state, State),
	State.

%------------------------------------------------------------------------------
pri_server_emu_done() ->
	?assertNot(pri_server_emu_has(reply)),
	State = pri_server_emu_get(state),
	erlang:apply(?MODULE, terminate, [State, 'pri_server_emulation_completed']),
	catch ets:delete(?PRI_SERVER_EMU_TABLE),
	ok = meck:unload(pri_server).

%------------------------------------------------------------------------------
pri_server_emu_has(Param) ->
	[] =/= ets:lookup(?PRI_SERVER_EMU_TABLE, Param).

%------------------------------------------------------------------------------
pri_server_emu_get(Param) ->
	[{Param, Value}] = ets:lookup(?PRI_SERVER_EMU_TABLE, Param),
	Value.

%------------------------------------------------------------------------------
pri_server_emu_pop(Param) ->
	?assert(pri_server_emu_has(Param)),
	Value = pri_server_emu_get(Param),
	pri_server_emu_clear(Param),
	Value.

%------------------------------------------------------------------------------
pri_server_emu_set(Param, Value) ->
	pri_server_emu_clear(Param),
	ets:insert_new(?PRI_SERVER_EMU_TABLE, {Param, Value}).

%------------------------------------------------------------------------------
pri_server_emu_clear(Param) ->
	ets:delete(?PRI_SERVER_EMU_TABLE, Param).

%------------------------------------------------------------------------------
pri_server_emu_server_reply(Client, Reply) ->
	?assertEqual(?PRI_SERVER_EMU_CLIENT, Client),
	?assertNot(pri_server_emu_has(reply)),
	pri_server_emu_set(reply, Reply).

%------------------------------------------------------------------------------
pri_server_emu_server_call(ServerRef, Request) ->
	pri_server_emu_server_call(ServerRef, Request, 5000).

pri_server_emu_server_call(ServerRef, Request, Timeout) ->
	pri_server_emu_server_call(ServerRef, Request, Timeout, normal).

pri_server_emu_server_call(ServerRef, Request, Timeout, Priority) ->
	?assert((is_integer(Timeout) andalso Timeout>0) or (infinity == Timeout)),
	?assert((low == Priority) or (normal == Priority) or (high == Priority)),
	?assert(?MODULE == ServerRef), % supports yet ONLY local,

	?assertNot(pri_server_emu_has(reply)), % pri_server_emu_clear(reply),
	State = pri_server_emu_get(state),

	{Reply1, State1} = case erlang:apply(?MODULE, handle_call, [Request, ?PRI_SERVER_EMU_CLIENT, State]) of
		{reply, Reply, NewState} ->
			?assertNot(pri_server_emu_has(reply)),
			{Reply, NewState};
		{reply, Reply, NewState, Timeout} ->
			?assert(is_integer(Timeout) and (Timeout>0)),
			?assertNot(pri_server_emu_has(reply)),
			{Reply, NewState};
		{reply,Reply,NewState,hibernate} ->
			?assertNot(pri_server_emu_has(reply)),
			{Reply, NewState};
		{noreply,NewState} ->
			{pri_server_emu_pop(reply), NewState};
		{noreply,NewState,Timeout} ->
			?assert(is_integer(Timeout) and (Timeout>0)),
			{pri_server_emu_pop(reply), NewState};
		{noreply,NewState,hibernate} ->
			{pri_server_emu_pop(reply), NewState};
		{stop,_Reason,Reply,NewState} ->
			?debugFmt("Server pri_server_emulation stopped in ~w with ~w", [?MODULE, _Reason]),
			?assertNot(pri_server_emu_has(reply)),
			{Reply,NewState};
		{stop,_Reason,NewState} ->
			?debugFmt("Server pri_server_emulation stopped in ~w with ~w", [?MODULE, _Reason]),
			{pri_server_emu_pop(reply), NewState};
		_Any ->
			?assert(false)
	end,
	pri_server_emu_set(state, State1),
	Reply1.

%------------------------------------------------------------------------------
pri_server_emu_server_cast(ServerRef, Request) ->
	pri_server_emu_server_cast(ServerRef, Request, normal).

pri_server_emu_server_cast(ServerRef, Request, Priority) ->
	?assert((low == Priority) or (normal == Priority) or (high == Priority)),
	?assert(?MODULE == ServerRef), % supports yet ONLY local,

	?assertNot(pri_server_emu_has(reply)), % pri_server_emu_clear(reply),
	State = pri_server_emu_get(state),

	State1 = case erlang:apply(?MODULE, handle_cast, [Request, State]) of
		{noreply,NewState} ->
			?assertNot(pri_server_emu_has(reply)),
			NewState;
		{noreply,NewState,Timeout} when is_integer(Timeout) ->
			?assert(is_integer(Timeout) and (Timeout>0)),
			?assertNot(pri_server_emu_has(reply)),
			NewState;
		{noreply,NewState,hibernate} ->
			?assertNot(pri_server_emu_has(reply)),
			NewState;
		{stop,_Reason,NewState} ->
			?debugFmt("Server pri_server_emulation stopped in ~w with ~w", [?MODULE, _Reason]),
			?assertNot(pri_server_emu_has(reply)),
			NewState;
		_Any ->
			?assert(false)
	end,
	pri_server_emu_set(state, State1),
	ok.

-endif.
