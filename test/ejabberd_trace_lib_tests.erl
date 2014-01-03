-module(ejabberd_trace_lib_tests).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("src/ejabberd_trace_lib.hrl").

-define(_a(Condition), ?_assert(Condition)).
-define(_eq(E, I), ?_assertEqual(E, I)).
-define(a(Condition), ?assert(Condition)).

%%
%% Tests
%%

c2s_trigger_test() ->
    T = c2s_trace(),
    ?a(if
           ?IS_C2S_TRIGGER(T) -> true;
           true -> false
       end).

bosh_trigger_test() ->
    T = bosh_trace(),
    ?a(if
           ?IS_BOSH_TRIGGER(T) -> true;
           true -> false
       end).

get_bind_result_test_() ->
    G = fun ejabberd_trace_lib:get_bind_result/1,
    [?_a(is_bind_result(G(c2s_trace()))),
     ?_a(is_bind_result(G(bosh_trace())))].

get_jid_test_() ->
    G = fun ejabberd_trace_lib:get_bind_result/1,
    J = fun ejabberd_trace_lib:get_jid/1,
    [?_eq(<<"asd@localhost/x3">>, J(G(c2s_trace()))),
     ?_eq(<<"carol@localhost/escalus-default-resource">>, J(G(bosh_trace())))].

is_bind_result({_, _, _, [{_, _, _, [_JidEl]}]} = BindIQResult)
  when ?IS_BIND_RESULT(BindIQResult) ->
    true;
is_bind_result(_) ->
    false.

%%
%% Fixtures
%%

c2s_trace() ->
    {trace,pid_port_fun_ref,call,
     {ejabberd_c2s,send_element,
      [{state,
        {socket_state,ejabberd_tls,
         {tlssock,pid_port_fun_ref,pid_port_fun_ref},
         pid_port_fun_ref},
        ejabberd_socket,pid_port_fun_ref,false,"3764444141",
        {sasl_state,<<"jabber">>,<<"localhost">>,<<>>,
         pid_port_fun_ref,
         pid_port_fun_ref,
         pid_port_fun_ref,cyrsasl_digest,
         {state,5,"3011314780",<<"asd">>,<<>>,
          pid_port_fun_ref,
          pid_port_fun_ref,
          ejabberd_auth_internal,<<"localhost">>}},
        c2s,c2s_shaper,false,true,false,true,
        [verify_none,{certfile,"/tmp/server.pem"}],
        true,undefined,<<"asd">>,<<"localhost">>,<<>>,undefined,
        {0,nil},
        {0,nil},
        {0,nil},
        {0,nil},
        [],undefined,undefined,undefined,false,
        {userlist,none,[],false},
        unknown,ejabberd_auth_internal,
        {{127,0,0,1},61957},
        [],"en"},
       {xmlel,<<"iq">>,
        [{<<"id">>,<<"bind_1">>},{<<"type">>,<<"result">>}],
        [{xmlel,<<"bind">>,
          [{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-bind">>}],
          [{xmlel,<<"jid">>,[],
            [{xmlcdata,<<"asd@localhost/x3">>}]}]}]}]}}.

bosh_trace() ->
    {trace,pid_port_fun_ref,send,
     {bosh_reply,
      {xmlel,<<"body">>,
       [{<<"sid">>,<<"dcac3f89e562c670d31ad7cc22f8504103a63b19">>},
        {<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>}],
       [{xmlel,<<"iq">>,
         [{<<"id">>,<<"370c1c72e14f20dd1262862343f1031a">>},
          {<<"type">>,<<"result">>}],
         [{xmlel,<<"bind">>,
           [{<<"xmlns">>,
             <<"urn:ietf:params:xml:ns:xmpp-bind">>}],
           [{xmlel,<<"jid">>,[],
             [{xmlcdata,
               <<"carol@localhost/escalus-default-resource">>}]}]}]}]}},
     pid_port_fun_ref}.
