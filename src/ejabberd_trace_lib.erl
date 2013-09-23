-module(ejabberd_trace_lib).

-export([get_env/3,
         extract_jid/1 ]).

%% dbg handler
-export([trace_handler/2]).

-include("ejabberd_trace_internal.hrl").

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

-define(IS_IQ(XML, IQ),
        XML =:= xmlel orelse XML =:= xmlelement,
        IQ =:= "iq" orelse IQ =:= <<"iq">>).

-define(IS_BIND(XML, Bind),
        XML =:= xmlel orelse XML =:= xmlelement,
        Bind =:= "bind" orelse Bind =:= <<"bind">>).

-spec extract_jid(ejt_xmlelement()) -> ejt_jid() | false.
extract_jid({XML, IQ, _, [{XML2, Bind, _, [{_, _, _, [{xmlcdata, Jid}]}]}]})
  when ?IS_IQ(XML, IQ) andalso ?IS_BIND(XML2, Bind) ->
    io:format(">>>>> found Jid: ~p~n", [Jid]),
    Jid;
extract_jid(_) ->
    io:format(">>>>> found no Jid~n", []),
    false.

trace_handler({trace, Pid, call,
               {ejabberd_c2s, send_element, [_, BindResult]}} = T,
              {Handler, TraceServer} = TState) ->
    io:format(">>>>> caught send_element~n", []),
    handle_trace(T, Pid, TState),
    case extract_jid(BindResult) of
        false ->
            ok;
        Jid ->
            do_trace_user(Jid, Pid, Handler, TraceServer)
    end,
    TState;
trace_handler(Trace, TState) ->
    handle_trace(Trace, element(2, Trace), TState),
    TState.

handle_trace(Trace, Pid, {Handler, TraceServer}) ->
    Action = ejabberd_trace_server:get_action(TraceServer, Pid),
    do_handle_trace(Action, Trace, Handler).

do_handle_trace(drop, _Trace, _Handler) ->
    ok;
do_handle_trace(cache, Trace, _Handler) ->
    cache_trace(Trace);
do_handle_trace(trace, Trace, Handler) ->
    Handler(Trace, user).

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

do_trace_user(Jid, Pid, Handler, TraceServer) ->
    io:format(">>>>> fake trace: ~p ~p~n", [Jid, Pid]),
    %% TODO: this Jid comes from unpacking an XML stanza
    %%       so may require unescaping
    case ets:lookup(?NEW_TRACES, Jid) of
        [] ->
            io:format(">>>>> fake trace: not tracing~n", []),
            ok;
        [{Jid, _Flags}] ->
            io:format(">>>>> fake trace: tracing ~n", []),
            ets:delete(?NEW_TRACES, Jid),
            flush_cache(Pid, Handler),
            maybe_disable_cache(TraceServer),
            TraceServer ! {traced_new_user, Jid, Pid}
    end.

flush_cache(Pid, Handler) ->
    io:format(">>>>> flush cache: ~p: ", [Pid]),
    case ets:lookup(?TRACE_CACHE, Pid) of
        [] ->
            io:format("no traces for ~p~n", [Pid]),
            ok;
        [{Pid, Traces}] ->
            io:format("~p traces~n", [length(Traces)]),
            [Handler(Trace, user) || Trace <- lists:reverse(Traces)],
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

extract_jid_test() ->
    Jid = "qwe@localhost/x3",
    ?assertEqual(Jid, extract_jid(example_bind(Jid))).

-endif.
