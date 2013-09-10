-module(ejabberd_trace).

%% API
-export([user/1]).

%% Types
-type ejdtrace_jid() :: string().
-type ejdtrace_string_type() :: list | binary.

%%
%% API
%%

%% @doc Trace a user given his/her bare JID.
%% @end
-spec user(JID) -> ok when
      JID :: ejdtrace_jid().
user(JID) ->
    user(JID, m).

%%
%% Internal functions
%%

user(JID, Flags) ->
    %% TODO: use ejabberd_sm to get the session list!
    {User, Domain} = jid_to_us(JID),
    case ets:select(session, match_session_pid(User, Domain)) of
        [] ->
            {error, not_found};
        [{_, C2SPid}] ->
            dbg:p(C2SPid, Flags);
        [_|_] = Sessions ->
            {error, {multiple_sessions, Sessions}}
    end.

jid_to_us(JID) ->
    jid_to_us(application:get_env(ejabberd_trace, string_type, list), JID).

-spec jid_to_us(StringType, JID) -> {User, Domain} when
      StringType :: ejdtrace_string_type(),
      JID :: ejdtrace_jid(),
      User :: list() | binary(),
      Domain :: list() | binary().
jid_to_us(list, JID) ->
    case string:tokens(JID, "@/") of
        [User, Domain, _] ->
            {User, Domain};
        [User, Domain] ->
            {User, Domain}
    end;
jid_to_us(binary, JID) ->
    {User, Domain} = jid_to_us(list, JID),
    {list_to_binary(User), list_to_binary(Domain)}.

match_session_pid(User, Domain) ->
    [{%% match pattern
      set(session(), [{2, {'_', '$1'}},
                      {3, '$2'},
                      {4, {User, Domain}}]),
      %% guards
      [],
      %% return
      [{'$2', '$1'}]}].

session() ->
    erlang:make_tuple(6, undefined).

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
