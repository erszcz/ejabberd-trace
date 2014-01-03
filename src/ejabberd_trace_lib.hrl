-ifndef(ejabberd_trace_lib_747e).
-define(ejabberd_trace_lib_747e, true).

-define(EL(N, Tuple), element(N, Tuple)).

-define(IS_XMLEL(El),
        size(El) =:= 4,
        ?EL(1, El) =:= xmlel orelse ?EL(1, El) =:= xmlelement,
        is_list(?EL(3, El)),
        is_list(?EL(4, El))).

-define(IS_IQ(El),
        ?IS_XMLEL(El),
        ?EL(2, El) =:= "iq" orelse ?EL(2, El) =:= <<"iq">>).

-define(IS_BIND(El),
        ?IS_XMLEL(El),
        ?EL(2, El) =:= "bind" orelse ?EL(2, El) =:= <<"bind">>).

-define(IS_JID(El),
        ?IS_XMLEL(El),
        ?EL(2, El) =:= "jid" orelse ?EL(2, El) =:= <<"jid">>).

-define(IS_C2S_TRIGGER(T),
        size(T) == 4,
        {trace, call} == {?EL(1, T), ?EL(3, T)},
        {ejabberd_c2s, send_element} == {?EL(1, ?EL(4, T)),
                                         ?EL(2, ?EL(4, T))}).

-endif. %% not defined ejabberd_trace_lib_747e
