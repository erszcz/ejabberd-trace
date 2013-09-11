-module(ejabberd_trace).

%% API
-export([user/1]).

-include("ejabberd_trace_internal.hrl").

%%
%% API
%%

%% @doc Trace an already logged in user given his/her JID.
-spec user(ejt_jid()) -> ok.
user(JID) ->
    user(JID, m).

%%
%% Internal functions
%%

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
