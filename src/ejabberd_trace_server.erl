-module(ejabberd_trace_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         trace_new_user/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% dbg handler
-export([trace_handler/2]).

-include("ejabberd_trace_internal.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%
%% API
%%

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec trace_new_user(ejt_jid(), [dbg_flag()]) -> any().
trace_new_user(JID, Flags) ->
    gen_server:call(?SERVER, {trace_new_user, JID, Flags}).

%%
%% gen_server callbacks
%%

init([]) ->
    traced_jids = ets:new(traced_jids, [named_table, public]),
    setup_tracer(),
    {ok, #state{}}.

handle_call({trace_new_user, JID, Flags}, _From, State) ->
    NewState = handle_trace_new_user(JID, Flags, State),
    {noreply, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions
%%

setup_tracer() ->
    %% TODO: Is the tracer running? Save the current trace patterns.
    dbg:tracer({process, {fun trace_handler/2, fun dbg:dhandler/2}}),
    dbg:p(get_c2s_sup(), [c, m, sos]),
    dbg:tpl(ejabberd_c2s, send_text, x),
    dbg:tpl(ejabberd_c2s, send_element, x).

get_c2s_sup() ->
    erlang:whereis(ejabberd_c2s_sup).

%% Assume the tracer is already started and knows what to do.
%% What does this handler do?
%% It only adds one more JID/Flags to the to-be-traced set.
handle_trace_new_user(JID, Flags, State) ->
    ets:insert(traced_jids, {JID, Flags}),
    State.

trace_handler({trace, Pid, call,
               {ejabberd_c2s, send_element, [_, BindResult]}} = T, Handler) ->
    Handler(T),
    cache_trace(T),
    case ?LIB:extract_jid(BindResult) of
        false ->
            ok;
        JID ->
            do_trace_user(JID, Pid, Handler)
    end,
    Handler;
trace_handler(Trace, Handler) ->
    Handler(Trace),
    cache_trace(Trace),
    Handler.

cache_trace(_Trace) ->
    %% TODO: actually do cache
    ok.

do_trace_user(Jid, Pid, _Handler) ->
    io:format(">>>>> fake trace: ~p ~p~n", [Jid, Pid]).
