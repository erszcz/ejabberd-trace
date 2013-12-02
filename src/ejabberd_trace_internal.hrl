-define(LIB, ejabberd_trace_lib).
-define(NEW_TRACES, ejt_new_traces).
-define(TRACE_CACHE, ejt_trace_cache).
-define(ACTIONS, ejt_actions).

-record(tstate, {filter,
                 format,
                 server}).
