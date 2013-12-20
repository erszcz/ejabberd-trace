-module(ejabberd_trace_tests).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(_ae(Class, Reason, Call), ?_assertException(Class, Reason, Call)).
-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).

stop_test() ->
    ejabberd_trace:start(),
    ejabberd_trace:stop([]),
    ?eq(false, is_running(ejabberd_trace)).

new_user_test_() ->
    NU3 = fun ejabberd_trace:new_user/3,
    NU5 = fun ejabberd_trace:new_user/5,
    Fmt = fun ejabberd_trace_format:stream/2,
    [?_ae(error, badarg, NU3("a@localhost/r", not_a_filter, Fmt)),
     ?_ae(error, badarg, NU5("a@localhost/r", m, [], not_a_filter, Fmt))].

is_running(App) ->
    AppInfo = application:info(),
    lists:keymember(App, 1, proplists:get_value(running, AppInfo)).
