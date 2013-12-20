-module(ejabberd_trace_filter).

%% Types
-export_type([filter/0,
              predefined_filter/0,
              filter_fun/0,
              trace/0]).

-type filter() :: (predefined_filter() |
                   {fn, filter_fun()} |
                   {any, [filter()]} |
                   {all, [filter()]}).

-type predefined_filter() :: (all |
                              rx |
                              tx |
                              tx_text |
                              tx_element |
                              routed_in |
                              routed_out |
                              stream).

-define(FILTER_FUN_DEF, (trace()) -> boolean()).
-type filter_fun() :: fun(?FILTER_FUN_DEF).

-type trace() :: any().

%% Predefined filters
-export([all/1,
         rx/1,
         tx/1,
         tx_text/1,
         tx_element/1,
         routed_out/1,
         routed_in/1,
         stream/1]).

%% API
-export([apply/2,
         is_filter/1]).

%%
%% API
%%

-spec apply(filter(), trace()) -> boolean().
apply({any, Filters}, Trace) ->
    lists:any(fun (F) -> ?MODULE:apply(F, Trace) end, Filters);
apply({all, Filters}, Trace) ->
    lists:all(fun (F) -> ?MODULE:apply(F, Trace) end, Filters);
apply({fn, FilterFun}, Trace) when is_function(FilterFun, 1) ->
    FilterFun(Trace);
apply(PredefinedFilter, Trace) when is_atom(PredefinedFilter) ->
    ?MODULE:PredefinedFilter(Trace).

-spec is_filter(any()) -> boolean().
is_filter(all) -> true;
is_filter(rx) -> true;
is_filter(tx) -> true;
is_filter(tx_text) -> true;
is_filter(tx_element) -> true;
is_filter(routed_in) -> true;
is_filter(routed_out) -> true;
is_filter(stream) -> true;
is_filter({any, Filters}) -> lists:all(fun is_filter/1, Filters);
is_filter({all, Filters}) -> lists:all(fun is_filter/1, Filters);
is_filter({fn, FilterFun}) when is_function(FilterFun, 1) -> true;
is_filter(_) -> false.

%%
%% Predefined filters
%%

-spec all/1 :: ?FILTER_FUN_DEF.
all(_) -> true.

-spec rx/1 :: ?FILTER_FUN_DEF.
rx(end_of_trace) -> true;
rx({trace, _Pid, 'receive', {'$gen_event', {xmlstreamstart, _, _}}}) -> true;
rx({trace, _Pid, 'receive', {'$gen_event', {ElemOrEnd, _}}})
  when ElemOrEnd == xmlstreamelement; ElemOrEnd == xmlstreamend -> true;
rx(_) -> false.

-spec tx/1 :: ?FILTER_FUN_DEF.
tx(end_of_trace) -> true;
tx({trace, _Pid, call, {ejabberd_c2s, send_text,
                        [_State, "<?xml version=" ++ _Rest]}}) -> true;
tx({trace, _Pid, call, {ejabberd_c2s, send_text,
                        [_State, "</stream:stream>"]}}) -> true;
tx({trace, _Pid, call, {ejabberd_c2s, send_element, [_State, _Msg]}}) -> true;
tx(_) -> false.

-spec tx_text/1 :: ?FILTER_FUN_DEF.
tx_text(end_of_trace) -> true;
tx_text({trace, _Pid, call, {ejabberd_c2s, send_text, [_State, _Msg]}}) -> true;
tx_text(_) -> false.

-spec tx_element/1 :: ?FILTER_FUN_DEF.
tx_element(end_of_trace) -> true;
tx_element({trace, _Pid, call, {ejabberd_c2s, send_element, [_State, _Msg]}}) -> true;
tx_element(_) -> false.

-spec routed_out/1 :: ?FILTER_FUN_DEF.
routed_out(end_of_trace) -> true;
routed_out({trace, _Pid, send, {route, _From, _To, _Packet}, _ToPid}) -> true;
routed_out(_) -> false.

-spec routed_in/1 :: ?FILTER_FUN_DEF.
routed_in(end_of_trace) -> true;
routed_in({trace, _Pid, 'receive', {route, _From, _To, _Packet}}) -> true;
routed_in(_) -> false.

-spec stream/1 :: ?FILTER_FUN_DEF.
stream(Trace) ->
    ?MODULE:apply({any, [rx, tx]}, Trace).
