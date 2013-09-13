-module(ejabberd_trace).

-behaviour(application).

%% API
-export([tracer/0,
         new_user/1,
         user/1]).

%% `application' callbacks
-export([start/2,
         stop/1]).

-include("ejabberd_trace_internal.hrl").

%%
%% API
%%

tracer() ->
    %% TODO: Is the tracer running? Save the current trace patterns.
    dbg:tracer(process, {fun ?LIB:trace_handler/2, fun dbg:dhandler/2}),
    dbg:p(get_c2s_sup(), [c, m, sos]),
    dbg:tpl(ejabberd_c2s, send_text, x),
    dbg:tpl(ejabberd_c2s, send_element, x),
    ok.

%% @doc Trace a user who is to connect in near future once he/she connects.
%% This intends to trace *all* of the communication of a specific connection.
%%
%% The usage scenario is as follows:
%% 1) `new_user/1,2' is called (e.g. from the shell) - the call blocks,
%% 2) the user connects,
%% 3) the call returns while the connection process responsible for `JID'
%%    is being traced.
%%
%% The magic happens in step (2) as the connection process is involved
%% and also more users than the one expected might connect.
%% This function takes care of determining who of those who connected
%% to trace based on his/her `JID'.
%% In order to do that some stanza with the JID must already be sent
%% on the connection - only then the matching may success.
%% This function buffers all debug messages up to the point of receiving
%% that stanza; then it forwards all the buffered messages for the traced user
%% and discards all the rest.
new_user(JID) ->
    new_user(JID, m).

%% @doc Trace an already logged in user given his/her JID.
-spec user(ejt_jid()) -> ok.
user(JID) ->
    user(JID, m).

%%
%% `application' callbacks
%%

start(_StartType, _Args) ->
    ejabberd_trace_sup:start_link().

stop(_) ->
    ok.

%%
%% Internal functions
%%

-spec get_c2s_sup() -> pid() | undefined.
get_c2s_sup() ->
    erlang:whereis(ejabberd_c2s_sup).

-spec new_user(ejt_jid(), [dbg_flag()]) -> any().
new_user(JID, Flags) ->
    ejabberd_trace_server:trace_new_user(JID, Flags).

-spec user(ejt_jid(), [dbg_flag()]) -> {ok, any()} |
                                       {error, not_found} |
                                       {error, {multiple_sessions, list()}} |
                                       {error, any()}.
user(JID, Flags) ->
    %% TODO: use ejabberd_sm to get the session list!
    UserSpec = parse_jid(JID),
    MatchSpec = match_session_pid(UserSpec),
    error_logger:info_msg("Session match spec: ~p~n", [MatchSpec]),
    case ets:select(session, MatchSpec) of
        [] ->
            {error, not_found};
        [{_, C2SPid}] ->
            dbg:p(C2SPid, Flags);
        [C2SPid] ->
            dbg:p(C2SPid, Flags);
        [_|_] = Sessions ->
            {error, {multiple_sessions, Sessions}}
    end.

parse_jid(JID) ->
    parse_jid(?LIB:get_env(ejabberd_trace, string_type, list), JID).

-spec parse_jid(StringType, JID) -> {User, Domain, Resource} |
                                    {User, Domain} when
      StringType :: ejt_string_type(),
      JID :: ejt_jid(),
      User :: list() | binary(),
      Domain :: list() | binary(),
      Resource :: list() | binary().
parse_jid(list, JID) ->
    case string:tokens(JID, "@/") of
        [User, Domain, Resource] ->
            {User, Domain, Resource};
        [User, Domain] ->
            {User, Domain}
    end;
parse_jid(binary, JID) ->
    list_to_tuple([list_to_binary(E)
                   || E <- tuple_to_list(parse_jid(list, JID))]).

-spec match_session_pid(UserSpec) -> ets:match_spec() when
      UserSpec :: {string(), string(), string()} | {string(), string()}.
match_session_pid({_User, _Domain, _Resource} = UDR) ->
    [{%% match pattern
      set(session(), [{2, {'_', '$1'}},
                      {3, UDR}]),
      %% guards
      [],
      %% return
      ['$1']}];

match_session_pid({User, Domain}) ->
    [{%% match pattern
      set(session(), [{2, {'_', '$1'}},
                      {3, '$2'},
                      {4, {User, Domain}}]),
      %% guards
      [],
      %% return
      [{{'$2', '$1'}}]}].

session() ->
    set(erlang:make_tuple(6, '_'), [{1, session}]).

%% @doc Set multiple fields of a record in one call.
%% Usage:
%% set(Record, [{#record.field1, Val1},
%%              {#record.field1, Val2},
%%              {#record.field3, Val3}])
%% @end
set(Record, FieldValues) ->
    F = fun({Field, Value}, Rec) ->
                setelement(Field, Rec, Value)
        end,
    lists:foldl(F, Record, FieldValues).
