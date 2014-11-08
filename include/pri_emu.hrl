% PriServer meck for testing
%===============================================================================
% This module meck's the main pri_server functionality, as init/call/cast/terminate
%===============================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(EMU_SERVER,	'emu_server').
-define(EMU_CLIENT,	'emu_client').

%------------------------------------------------------------------------------
emu_init(Args) ->
	ok = meck:new(pri_server),

	ok = meck:expect(pri_server, call, fun emu_server_call/2),
	ok = meck:expect(pri_server, call, fun emu_server_call/3),
	ok = meck:expect(pri_server, call, fun emu_server_call/4),

	ok = meck:expect(pri_server, cast, fun emu_server_cast/2),
	ok = meck:expect(pri_server, cast, fun emu_server_cast/3),

	ok = meck:expect(pri_server, reply, fun emu_server_reply/2),

	State = emu_init([]),
	ets:new(?EMU_SERVER, [named_table, set, private]),
	Result = init(Args),
	?assertNot(emu_has(reply)),
	?assertMatch({ok, _State}, Result),
	{ok, State} = Result,
	emu_set(state, State),
	State.

%------------------------------------------------------------------------------
emu_done() ->
	?assertNot(emu_has(reply)),
	State = emu_get(state),
	terminate(State, 'emulation_completed'),
	catch ets:delete(?EMU_SERVER),
	ok = meck:unload(pri_server).

%------------------------------------------------------------------------------
emu_has(Param) ->
	[] =/= ets:lookup(?EMU_SERVER, Param).

%------------------------------------------------------------------------------
emu_get(Param) ->
	[{Param, Value}] = ets:lookup(?EMU_SERVER, Param),
	Value.

%------------------------------------------------------------------------------
emu_pop(Param) ->
	?assert(emu_has(Param)),
	Value = emu_get(Param),
	emu_clear(Param),
	Value.

%------------------------------------------------------------------------------
emu_set(Param, Value) ->
	emu_clear(Param),
	ets:insert_new(?EMU_SERVER, {Param, Value}).

%------------------------------------------------------------------------------
emu_clear(Param) ->
	ets:delete(?EMU_SERVER, Param).

%------------------------------------------------------------------------------
emu_server_reply(Client, Reply) ->
	?assertEqual(?EMU_CLIENT, Client),
	?assertNot(emu_has(reply)),
	emu_set(reply, Reply).

%------------------------------------------------------------------------------
emu_server_call(ServerRef, Request) ->
	emu_server_call(ServerRef, Request, 5000).

emu_server_call(ServerRef, Request, Timeout) ->
	emu_server_call(ServerRef, Request, Timeout, normal).

emu_server_call(ServerRef, Request, Timeout, Priority) ->
	?assert((is_integer(Timeout) andalso Timeout>0) or (infinity == Timeout)),
	?assert((low == Priority) or (normal == Priority) or (high == Priority)),
	?assert(?MODULE == ServerRef), % supports yet ONLY local,

	?assertNot(emu_has(reply)), % emu_clear(reply),
	State = emu_get(state),

	{Reply1, State1} = case handle_call(Request, ?EMU_CLIENT, State) of
		{reply, Reply, NewState} ->
			?assertNot(emu_has(reply)),
			{Reply, NewState};
		{reply, Reply, NewState, Timeout} ->
			?assert(is_integer(Timeout) and (Timeout>0)),
			?assertNot(emu_has(reply)),
			{Reply, NewState};
		{reply,Reply,NewState,hibernate} ->
			?assertNot(emu_has(reply)),
			{Reply, NewState};
		{noreply,NewState} ->
			{emu_pop(reply), NewState};
		{noreply,NewState,Timeout} ->
			?assert(is_integer(Timeout) and (Timeout>0)),
			{emu_pop(reply), NewState};
		{noreply,NewState,hibernate} ->
			{emu_pop(reply), NewState};
		{stop,_Reason,Reply,NewState} ->
			?debugFmt("Server emulation stopped in ~w with ~w", [?MODULE, _Reason]),
			?assertNot(emu_has(reply)),
			{Reply,NewState};
		{stop,_Reason,NewState} ->
			?debugFmt("Server emulation stopped in ~w with ~w", [?MODULE, _Reason]),
			{emu_pop(reply), NewState};
		_Any ->
			?assert(false)
	end,
	emu_set(state, State1),
	Reply1.

%------------------------------------------------------------------------------
emu_server_cast(ServerRef, Request) ->
	emu_server_cast(ServerRef, Request, normal).

emu_server_cast(ServerRef, Request, Priority) ->
	?assert((low == Priority) or (normal == Priority) or (high == Priority)),
	?assert(?MODULE == ServerRef), % supports yet ONLY local,

	?assertNot(emu_has(reply)), % emu_clear(reply),
	State = emu_get(state),

	State1 = case handle_cast(Request, State) of
		{noreply,NewState} ->
			?assertNot(emu_has(reply)),
			NewState;
		{noreply,NewState,Timeout} when is_integer(Timeout) ->
			?assert(is_integer(Timeout) and (Timeout>0)),
			?assertNot(emu_has(reply)),
			NewState;
		{noreply,NewState,hibernate} ->
			?assertNot(emu_has(reply)),
			NewState;
		{stop,_Reason,NewState} ->
			?debugFmt("Server emulation stopped in ~w with ~w", [?MODULE, _Reason]),
			?assertNot(emu_has(reply)),
			NewState;
		_Any ->
			?assert(false)
	end,
	emu_set(state, State1),
	ok.

-endif.
