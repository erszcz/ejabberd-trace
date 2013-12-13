-module(ejabberd_trace_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(eq(E, I), ?assertEqual(E, I)).

stop_test() ->
    ejabberd_trace:start(),
    ejabberd_trace:stop([]),
    ?eq(false, is_running(ejabberd_trace)).

is_running(App) ->
    AppInfo = application:info(),
    lists:keymember(App, 1, proplists:get_value(running, AppInfo)).
