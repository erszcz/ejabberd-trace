-define(LIB, ejabberd_trace_lib).
-define(NEW_TRACES, ejt_new_traces).

-type dbg_flag() :: s | r | m | c | p | sos | sol | sofs | sofl | all | clear.

-type ejt_jid() :: string().
-type ejt_string_type() :: list | binary.
-type ejt_xmlelement() :: any().
