-module(ejabberd_trace).

-behaviour(application).

%% API
-export([new_user/1, new_user/2, new_user/3, new_user/5,
         user/1,
         state/1,
         string_type/0, string_type/1]).

%% `application' callbacks
-export([start/2,
         stop/1]).

%% Types
-type dbg_flag() :: s | r | m | c | p | sos | sol | sofs | sofl | all | clear.
-export_type([dbg_flag/0]).

-type sys_status() :: {status, pid(), {module, module()}, [any()]}.
-export_type([sys_status/0]).

-type jid() :: string().
-export_type([jid/0]).

-type string_type() :: list | binary.
-export_type([string_type/0]).

-type xmlelement() :: any().
-export_type([xmlelement/0]).

-type filter() :: ejabberd_trace_filter:filter().
-export_type([filter/0]).

-type formatter() :: fun().

-include("ejabberd_trace_internal.hrl").

%%
%% API
%%

%% @doc `new_user/1,2,3,5' traces a user who is to connect in near
%% future once he/she connects.
%% This intends to trace *all* of the communication of a specific connection.
%%
%% The usage scenario is as follows:
%% 1) `new_user/1,2,3,5' is called (e.g. from the shell);
%%    all c2s connections to the server established from this moment
%%    are traced and the messages they receive analysed for the presence
%%    of the JID in question,
%% 2) the expected user connects,
%% 3) all buffered traces except those of the expected user are discarded;
%%    traces of that user are filtered with the defined filter and passed
%%    to the chosen formatter;
%% 4) the tracing continues as it would with `dbg'.
%% @end

-spec new_user(jid()) -> any() | no_return().
new_user(Jid) ->
    new_user(Jid, m).

-spec new_user(jid(), filter()) -> any() | no_return().
new_user(Jid, Filter) ->
    Format = fun (Trace, _Opts) ->
                     dbg:dhandler(Trace, user)
             end,
    new_user(Jid, m, [], Filter, Format).

-spec new_user(jid(), filter(), formatter()) -> any() | no_return().
new_user(Jid, Filter, Format) ->
    new_user(Jid, m, [], Filter, Format).

-spec new_user(jid(), [dbg_flag()], [node()], filter(), formatter())
    -> any() | no_return().
new_user(Jid, Flags, Nodes, Filter, Format) ->
    ejabberd_trace_filter:is_filter(Filter) orelse
    begin
        Args = [Jid, Flags, Nodes, Filter, Format],
        error(badarg, Args)
    end,
    maybe_start_dbg(is_dbg_running(), Nodes, Filter, Format),
    ejabberd_trace_server:trace_new_user(fix_string(Jid), Flags).

%% @doc Trace an already logged in user given his/her Jid.
%% @end

-spec user(jid()) -> {ok, any()} |
                     {error, not_found} |
                     {error, {multiple_sessions, list()}} |
                     {error, any()}.
user(Jid) ->
    user(Jid, m).

-spec user(jid(), [dbg_flag()]) -> {ok, any()} |
                                   {error, not_found} |
                                   {error, {multiple_sessions, list()}} |
                                   {error, any()}.
user(Jid, Flags) ->
    is_dbg_running() orelse dbg:tracer(),
    %% TODO: use ejabberd_sm to get the session list!
    UserSpec = parse_jid(Jid),
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

%% @doc Return sys:get_status/1 result of the process corresponding to Jid.
%% @end

-spec state(jid()) -> sys_status().
state(Jid) ->
    UserSpec = parse_jid(Jid),
    MatchSpec = match_session_pid(UserSpec),
    error_logger:info_msg("Session match spec: ~p~n", [MatchSpec]),
    case ets:select(session, MatchSpec) of
        [] ->
            {error, not_found};
        [{_, C2SPid}] ->
            sys:get_status(C2SPid);
        [C2SPid] ->
            sys:get_status(C2SPid);
        [_|_] = Sessions ->
            {error, {multiple_sessions, Sessions}}
    end.

string_type(StringType) ->
    application:set_env(ejabberd_trace, string_type, StringType).

string_type() ->
    ?LIB:get_env(ejabberd_trace, string_type, list).

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

is_dbg_running() ->
    case erlang:whereis(dbg) of
        Pid when is_pid(Pid) -> true;
        _ -> false
    end.

maybe_start_dbg(true, _Nodes, _Filter, _Format) ->
    case ?LIB:get_env(ejabberd_trace, my_dbg, false) of
        true ->
            ok;
        _ ->
            io:format("ejabberd_trace requires dbg to work, "
                      "but dbg is already running.~n"
                      "If you're sure it's safe to stop it use "
                      "dbg:stop_clear() and retry this call.~n"),
            error(dbg_running)
    end;

maybe_start_dbg(false, Nodes, Filter, Format) ->
    application:start(sasl),
    application:start(ejabberd_trace),
    TraceServer = erlang:whereis(ejabberd_trace_server),
    dbg:tracer(process, {fun ?LIB:trace_handler/2,
                         #tstate{filter = Filter,
                                 format = Format,
                                 server = TraceServer}}),
    [dbg:n(Node) || Node <- Nodes],
    dbg:p(get_c2s_sup(), [c, m, sos]),
    dbg:tpl(ejabberd_c2s, send_text, x),
    dbg:tpl(ejabberd_c2s, send_element, x),
    application:set_env(ejabberd_trace, my_dbg, true),
    ok.

parse_jid(Jid) ->
    parse_jid(string_type(), Jid).

-spec parse_jid(StringType, Jid) -> {User, Domain, Resource} |
                                    {User, Domain} when
      StringType :: string_type(),
      Jid :: jid(),
      User :: list() | binary(),
      Domain :: list() | binary(),
      Resource :: list() | binary().
parse_jid(list, Jid) ->
    case string:tokens(Jid, "@/") of
        [User, Domain, Resource] ->
            {User, Domain, Resource};
        [User, Domain] ->
            {User, Domain}
    end;
parse_jid(binary, Jid) ->
    list_to_tuple([list_to_binary(E)
                   || E <- tuple_to_list(parse_jid(list, Jid))]).

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

fix_string(String) ->
    fix_string(String, string_type()).

-spec fix_string(binary() | string(), string_type()) -> binary() | string().
fix_string(BString, binary) when is_binary(BString) -> BString;
fix_string(BString, list) when is_binary(BString) -> binary_to_list(BString);
fix_string(String, list) when is_list(String) -> String;
fix_string(String, binary) when is_list(String) -> list_to_binary(String).
