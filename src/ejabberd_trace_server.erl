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
    gen_server:call(?SERVER, {trace_new_user, JID, Flags}, timer:seconds(20)).

%%
%% gen_server callbacks
%%

init([]) ->
    traced_jids = ets:new(traced_jids, [named_table, public]),
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

%% Assume the tracer is already started and knows what to do.
%% What does this handler do?
%% It only adds one more JID/Flags to the to-be-traced set.
handle_trace_new_user(JID, Flags, State) ->
    ets:insert(traced_jids, {JID, Flags}),
    State.
