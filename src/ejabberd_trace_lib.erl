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
extract_jid({XML, IQ, _, [{XML2, Bind, _, [{_, _, _, [{xmlcdata, JID}]}]}]})
  when ?IS_IQ(XML, IQ) andalso ?IS_BIND(XML2, Bind) ->
    io:format(">>>>> found JID: ~p~n", [JID]),
    JID;
extract_jid(_) ->
    io:format(">>>>> found no JID~n", []),
    false.

trace_handler({trace, Pid, call,
               {ejabberd_c2s, send_element, [_, BindResult]}} = T,
              {Handler, TraceServer} = TState) ->
    io:format(">>>>> caught send_element~n", []),
    cache_trace(T, TraceServer),
    case extract_jid(BindResult) of
        false ->
            ok;
        JID ->
            do_trace_user(JID, Pid, Handler, TraceServer)
    end,
    TState;
trace_handler(Trace, {_, TraceServer} = TState) ->
    cache_trace(Trace, TraceServer),
    TState.

%% TODO: this is lame - message send/recv traces are stored twice
%% so will be duplicated when printing. Store everything once (no key) and
%% select appropriately when flushing the cache.
cache_trace({trace, From, _, _, To} = Trace, TraceServer) ->
    CacheEnabled = ejabberd_trace_server:get_cache(TraceServer),
    io:format(">>>>> cache trace: cache enabled = ~p~n", [CacheEnabled]),
    do_cache(CacheEnabled, From, Trace),
    do_cache(CacheEnabled, To, Trace);
cache_trace({trace, Pid, _, _} = Trace, TraceServer) ->
    CacheEnabled = ejabberd_trace_server:get_cache(TraceServer),
    io:format(">>>>> cache trace: cache enabled = ~p~n", [CacheEnabled]),
    do_cache(CacheEnabled, Pid, Trace).

do_cache(true, Pid, Trace) ->
    case ets:lookup(?TRACE_CACHE, Pid) of
        [] ->
            io:format(">>>>> do cache: first trace for ~p~n", [Pid]),
            ets:insert(?TRACE_CACHE, {Pid, [Trace]});
        [{Pid, Traces}] ->
            io:format(">>>>> do cache: appending trace for ~p~n", [Pid]),
            ets:insert(?TRACE_CACHE, {Pid, [Trace | Traces]})
    end;
do_cache(false, _, _) ->
    ok.

do_trace_user(Jid, Pid, Handler, TraceServer) ->
    io:format(">>>>> fake trace: ~p ~p~n", [Jid, Pid]),
    %% TODO: this Jid comes from unpacking an XML stanza
    %%       so may require unescaping
    case ets:lookup(?NEW_TRACES, Jid) of
        [] ->
            io:format(">>>>> fake trace: not tracing~n", []),
            ok;
        [{Jid, _Flags, From}] ->
            io:format(">>>>> fake trace: tracing ~n", []),
            ets:delete(?NEW_TRACES, Jid),
            flush_cache(Jid, Handler),
            maybe_disable_cache(TraceServer),
            TraceServer ! {traced_new_user, From, ok}
    end.

flush_cache(Jid, Handler) ->
    io:format(">>>>> flush cache: ~p: ", [Jid]),
    case ets:lookup(?TRACE_CACHE, Jid) of
        [] ->
            io:format("no traces for ~p~n", [Jid]),
            ok;
        [{Jid, Traces}] ->
            io:format("~p traces~n", [length(Traces)]),
            [Handler(Trace, user) || Trace <- lists:reverse(Traces)],
            ets:delete(?TRACE_CACHE, Jid)
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
