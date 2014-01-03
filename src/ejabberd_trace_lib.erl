-module(ejabberd_trace_lib).

-export([get_env/3,
         extract_jid/1 ]).

%% dbg handler
-export([trace_handler/2]).

-type xmlelement() :: {xmlel | xmlelement,
                       list() | binary(),
                       [{list() | binary(), list() | binary()}],
                       [any()]}.

-include("ejabberd_trace_internal.hrl").
-include("ejabberd_trace_lib.hrl").

%% @doc Return `Application' environment variable `Par'.
%% If no such variable is defined return `Def'.
%% This is modelled after `application:get_env/3' from R16B01.
get_env(Application, Par, Def) ->
    case application:get_env(Application, Par) of
        undefined ->
            Def;
        {ok, Val} ->
            Val
    end.

-spec extract_jid(xmlelement()) -> ejabberd_trace:jid() | false.
%% TODO: compiler crashes on this head, report bug
%% extract_jid({_, _, _, [{_, _, _, [Jid]} = Bind]} = BindIQResult)
%%   when ?IS_XMLEL(BindIQResult),
%%        ?IS_BIND(Bind),
%%        ?IS_JID(Jid) ->
extract_jid({_, _, _, [{_, _, _, [Jid]}]} = BindIQResult)
  when ?IS_IQ(BindIQResult),
       ?IS_BIND(hd(?EL(4, BindIQResult))),
       ?IS_JID(Jid) ->
    {_, _, _, [{xmlcdata, RealJid}]} = Jid,
    ?DEBUG(">>>>> found Jid: ~p~n", [RealJid]),
    RealJid;
extract_jid(_) ->
    ?DEBUG(">>>>> found no Jid~n", []),
    false.

trace_handler(Trace, #tstate{} = TState)
  when ?IS_C2S_TRIGGER(Trace) ->
    Pid = element(2, Trace),
    %% TODO: extract BindResult from Trace
    BindResult = 1/0,
    ?DEBUG(">>>>> c2s trigger matched pid=~p~n", [Pid]),
    handle_trace(Trace, Pid, TState),
    case extract_jid(BindResult) of
        false ->
            ok;
        Jid ->
            do_trace_user(Jid, Pid, TState)
    end,
    TState;
trace_handler(Trace, TState) ->
    handle_trace(Trace, element(2, Trace), TState),
    TState.

handle_trace(Trace, Pid, #tstate{server = TraceServer} = TState) ->
    Action = ejabberd_trace_server:get_action(TraceServer, Pid),
    do_handle_trace(Action, Trace, TState).

do_handle_trace(drop, _Trace, _TState) ->
    ok;
do_handle_trace(cache, Trace, _TState) ->
    cache_trace(Trace);
do_handle_trace(trace, Trace, TState) ->
    filter_and_format(Trace, TState).

filter_and_format(Trace, #tstate{filter = Filter,
                                 format = Format} = TState) ->
    case ejabberd_trace_filter:apply(Filter, Trace) of
        true ->
            Format(Trace, []),
            TState;
        false ->
            TState
    end.

%% TODO: this is lame - message send/recv traces are stored twice
%% so will be duplicated when printing. Store everything once (no key) and
%% select appropriately when flushing the cache.
cache_trace({trace, From, _, _, To} = Trace) ->
    do_cache(From, Trace),
    do_cache(To, Trace);
cache_trace({trace, Pid, _, _} = Trace) ->
    do_cache(Pid, Trace).

do_cache(Pid, Trace) ->
    case ets:lookup(?TRACE_CACHE, Pid) of
        [] ->
            ets:insert(?TRACE_CACHE, {Pid, [Trace]});
        [{Pid, Traces}] ->
            ets:insert(?TRACE_CACHE, {Pid, [Trace | Traces]})
    end.

do_trace_user(Jid, Pid, #tstate{server = TraceServer} = TState) ->
    ?DEBUG(">>>>> fake trace: ~p ~p~n", [Jid, Pid]),
    %% TODO: this Jid comes from unpacking an XML stanza
    %%       so may require unescaping
    case ets:lookup(?NEW_TRACES, Jid) of
        [] ->
            ?DEBUG(">>>>> fake trace: not tracing~n", []),
            ok;
        [{Jid}] ->
            ?DEBUG(">>>>> fake trace: tracing ~n", []),
            ets:delete(?NEW_TRACES, Jid),
            flush_cache(Pid, TState),
            maybe_disable_cache(TraceServer),
            TraceServer ! {traced_new_user, Jid, Pid}
    end.

flush_cache(Pid, #tstate{} = TState) ->
    ?DEBUG(">>>>> flush cache: ~p: ", [Pid]),
    case ets:lookup(?TRACE_CACHE, Pid) of
        [] ->
            ?DEBUG("no traces for ~p~n", [Pid]),
            ok;
        [{Pid, Traces}] ->
            ?DEBUG("~p traces~n", [length(Traces)]),
            [filter_and_format(Trace, TState)
             || Trace <- lists:reverse(Traces)],
            ets:delete(?TRACE_CACHE, Pid)
    end.

maybe_disable_cache(TraceServer) ->
    case ets:info(?NEW_TRACES, size) of
        0 ->
            ejabberd_trace_server:set_cache(TraceServer, false),
            ets:delete_all_objects(?TRACE_CACHE);
        _ ->
            ok
    end.

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

example_bind(Jid) ->
    {xmlelement, "iq",
     [{"id", "bind_1"},{"type", "result"}],
     [{xmlelement, "bind",
       [{"xmlns", "urn:ietf:params:xml:ns:xmpp-bind"}],
       [{xmlelement, "jid",[],
         [{xmlcdata, Jid}]}]}]}.

%% TODO
c2s_trigger_test() ->
    error(unimplemented).

%% TODO
bosh_trigger_test() ->
    error(unimplemented).

extract_jid_test() ->
    Jid = "qwe@localhost/x3",
    ?assertEqual(Jid, extract_jid(example_bind(Jid))).

-endif.
