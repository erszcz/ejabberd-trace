-module(ejabberd_trace_filter).

%% Filters
-export([raw_traces/1,
         rx/1,
         tx/1,
         routed_out/1,
         routed_in/1]).

%% Combinators
-export([any/1,
         all/1]).

%% API
-export([apply/2]).

%% Types
-type trace() :: any().
-export_type([trace/0]).

-define(FILTER_FUN_DEF, (trace()) -> boolean()).
-type filter_fun() :: fun(?FILTER_FUN_DEF).
-export_type([filter_fun/0]).

-opaque filter() :: ({any, [filter()]} |
                     {all, [filter()]} |
                     {filter, filter_fun()}).
-export_type([filter/0]).

-spec apply(filter(), trace()) -> boolean().
apply({any, [Filters]}, Trace) ->
    lists:any(fun (F) -> ?MODULE:apply(F, Trace) end, Filters);
apply({all, [Filters]}, Trace) ->
    lists:all(fun (F) -> ?MODULE:apply(F, Trace) end, Filters);
apply({filter, Fun}, Trace) ->
    Fun(Trace).

-spec any([atom() | filter() | filter_fun()]) -> filter().
any(Filters) ->
    {any, [get_filter(F) || F <- Filters]}.

-spec all([atom() | filter() | filter_fun()]) -> filter().
all(Filters) ->
    {all, [get_filter(F) || F <- Filters]}.

-spec get_filter(atom() | filter_fun() | filter()) -> filter().
get_filter(Filter) when is_atom(Filter) ->
    {filter, fun ?MODULE:Filter/1};
get_filter(Filter) when is_function(Filter, 1) ->
    {filter, Filter};
get_filter(Filter) ->
    case is_filter(Filter) of
        true ->
            Filter;
        false ->
            error(badarg, Filter)
    end.

is_filter({any, _}) -> true;
is_filter({all, _}) -> true;
is_filter({filter, _}) -> true;
is_filter(_) -> false.

%%
%% Filters
%%

-spec raw_traces/1 :: ?FILTER_FUN_DEF.
raw_traces(_) -> true.

-spec rx/1 :: ?FILTER_FUN_DEF.
rx(end_of_trace) -> true;
rx({trace, _Pid, 'receive', {'$gen_event', {xmlstreamstart, _, _}}}) -> true;
rx({trace, _Pid, 'receive', {'$gen_event', {ElemOrEnd, _}}})
  when ElemOrEnd == xmlstreamelement; ElemOrEnd == xmlstreamend -> true;
rx(_) -> false.

-spec tx/1 :: ?FILTER_FUN_DEF.
tx(end_of_trace) -> true;
tx({trace, _Pid, call, {ejabberd_c2s, send_text, [_State, _Msg]}}) -> true;
tx(_) -> false.

-spec routed_out/1 :: ?FILTER_FUN_DEF.
routed_out(end_of_trace) -> true;
routed_out({trace, _Pid, send, {route, _From, _To, _Packet}, _ToPid}) -> true;
routed_out(_) -> false.

-spec routed_in/1 :: ?FILTER_FUN_DEF.
routed_in(end_of_trace) -> true;
routed_in({trace, _Pid, 'receive', {route, _From, _To, _Packet}}) -> true;
routed_in(_) -> false.

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(_eq(E, I), ?_assertEqual(E, I)).

get_filter_test_() ->
    GT = fun get_filter/1,
    [?_eq({filter, fun ?MODULE:rx/1}, GT(rx)),
     ?_eq({filter, fun ?MODULE:rx/1}, GT(fun ?MODULE:rx/1)),
     ?_eq({filter, fun ?MODULE:rx/1}, GT({filter, fun ?MODULE:rx/1})),
     ?_assertError(badarg, GT({some, shit}))].

-endif.
