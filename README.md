 Erlang/Pri-Server
==================

MODULE
------
pri_server

MODULE SUMMARY
--------------
  Generic Server Behaviour with Priorities

DESCRIPTION
-----------
  *Pri-Server* is a OTP/GenServer with priorities and full compatible with original code. Information about gen_server be found at [erlang.org][1].

EXPORTS
-------
`call(ServerRef, Request, Timeout, Priority) -> Reply`
    ServerRef, Request, Timeout and Reply - see gen_server:call/3
    Priority = pri_server:priority() message's priority

`cast(ServerRef, Request, Priority) -> ok`
    Priority = pri_server:priority() message's priority

NOTES
-----
  Events 'system' and 'EXIT' have high priority
  handle_info's priority is low

REFS
----
    [1]: http://www.erlang.org
