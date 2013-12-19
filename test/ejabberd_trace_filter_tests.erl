-module(ejabberd_trace_filter_tests).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(_eq(E, I), ?_assertEqual(E, I)).

%%
%% Tests
%%

rx_test_() ->
    R = fun ejabberd_trace_filter:rx/1,
    [?_eq(true, R(rx_trace())),
     ?_eq(false, R(tx_text_trace())),
     ?_eq(false, R(routed_in_trace())),
     ?_eq(false, R(routed_out_trace()))].

tx_test_() ->
    T = fun ejabberd_trace_filter:tx/1,
    [?_eq(true, T(tx_element_trace())),
     ?_eq(true, T(tx_streamstart_trace())),
     ?_eq(true, T(tx_streamend_trace())),
     ?_eq(false, T(tx_text_trace())),
     ?_eq(false, T(rx_trace())),
     ?_eq(false, T(routed_in_trace())),
     ?_eq(false, T(routed_out_trace()))].

tx_text_test_() ->
    T = fun ejabberd_trace_filter:tx_text/1,
    [?_eq(true, T(tx_text_trace())),
     ?_eq(false, T(rx_trace())),
     ?_eq(false, T(routed_in_trace())),
     ?_eq(false, T(routed_out_trace()))].

tx_element_test_() ->
    T = fun ejabberd_trace_filter:tx_element/1,
    [?_eq(true, T(tx_element_trace())),
     ?_eq(false, T(tx_text_trace())),
     ?_eq(false, T(rx_trace())),
     ?_eq(false, T(routed_in_trace())),
     ?_eq(false, T(routed_out_trace()))].

routed_in_test_() ->
    RI = fun ejabberd_trace_filter:routed_in/1,
    [?_eq(true, RI(routed_in_trace())),
     ?_eq(false, RI(routed_out_trace())),
     ?_eq(false, RI(rx_trace())),
     ?_eq(false, RI(tx_text_trace()))].

routed_out_test_() ->
    RO = fun ejabberd_trace_filter:routed_out/1,
    [?_eq(true, RO(routed_out_trace())),
     ?_eq(false, RO(routed_in_trace())),
     ?_eq(false, RO(rx_trace())),
     ?_eq(false, RO(tx_text_trace()))].

stream_test_() ->
    S = fun ejabberd_trace_filter:stream/1,
    [?_eq(true, S(tx_streamstart_trace())),
     ?_eq(true, S(tx_streamend_trace())),
     ?_eq(true, S(tx_element_trace())),
     ?_eq(true, S(rx_trace())),
     ?_eq(false, S(routed_in_trace())),
     ?_eq(false, S(routed_out_trace()))].

apply_test_() ->
    Ap = fun ejabberd_trace_filter:apply/2,
    [?_eq(true, Ap(rx, rx_trace())),
     ?_eq(false, Ap(rx, tx_text_trace())),
     ?_eq(true, Ap({any, [rx, tx]}, rx_trace())),
     ?_eq(false, Ap({all, [rx, tx]}, rx_trace())),
     ?_eq(true, Ap({fn, fun ejabberd_trace_filter:rx/1}, rx_trace()))].

%%
%% Fixtures
%%

rx_trace() ->
    {trace,'some_pid','receive',
     {'$gen_event',
      {xmlstreamelement,
       {xmlelement,"iq",
        [{"xmlns","jabber:client"},
         {"type","get"},
         {"to","asd@localhost/x3"},
         {"id","ac6fa"}],
        [{xmlcdata,<<"\n">>},
         {xmlelement,"query",
          [{"xmlns","jabber:iq:version"}],
          []},
         {xmlcdata,<<"\n">>}]}}}}.

tx_text_trace() ->
    {trace,'some_pid',call,
     {ejabberd_c2s,send_text,
      [{state,
        {socket_state,gen_tcp,'some_port','some_pid'},
        ejabberd_socket,'some_ref',false,"3635346036",
        undefined,c2s,c2s_shaper,false,false,false,false,
        [verify_none],
        false,undefined,[],"localhost",[],undefined,
        {0,nil},
        {0,nil},
        {0,nil},
        {0,nil},
        undefined,undefined,undefined,false,
        {userlist,none,[],false},
        unknown,unknown,
        {{127,0,0,1},59226},
        [],undefined,false,0,0,[],0,100,1},
       <<"<stream:features><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>DIGEST-MD5</mechanism><mechanism>PLAIN</mechanism><mechanism>SCRAM-SHA-1</mechanism></mechanisms><c xmlns='http://jabber.org/protocol/caps' hash='sha-1' node='http://www.process-one.net/en/ejabberd/' ver='mfN6SdQ3DGO7/QUHHftElVDFZ7k='/><register xmlns='http://jabber.org/features/iq-register'/><sm xmlns='urn:xmpp:sm:3'/></stream:features>">>]}}.

tx_element_trace() ->
    {trace,'some_pid',call,
     {ejabberd_c2s,send_element,
      [{state,
        {socket_state,gen_tcp,'some_port','some_pid'},
        ejabberd_socket,'some_ref',false,"3618468885",
        {sasl_state,"jabber","localhost",[],
         'some_fun',
         'some_fun',
         'some_fun',undefined,undefined},
        c2s,c2s_shaper,false,false,false,false,
        [verify_none],
        true,
        {jid,"alice","localhost","escalus-default-resource",
         "alice","localhost","escalus-default-resource"},
        "alice","localhost","escalus-default-resource",
        {{1385,996502,24109},'some_pid'},
        {1,{{"alice","localhost",[]},nil,nil}},
        {1,{{"alice","localhost",[]},nil,nil}},
        {1,{{"alice","localhost",[]},nil,nil}},
        {0,nil},
        {xmlelement,"presence",[{"xml:lang","en"}],[]},
        undefined,
        {{2013,12,2},{15,1,42}},
        false,
        {userlist,none,[],false},
        c2s,ejabberd_auth_internal,
        {{127,0,0,1},64236},
        [],"en",true,1,0,
        [],
        3,100,never},
       {xmlelement,"presence",
        [{"from","alice@localhost/escalus-default-resource"},
         {"to","alice@localhost/escalus-default-resource"},
         {"xml:lang","en"}],
        []}]}}.

tx_streamstart_trace() ->
    {trace,'some_pid',call,
     {ejabberd_c2s,send_text,
      [{state,
        {socket_state,gen_tcp,'some_port','some_pid'},
        ejabberd_socket,'some_ref',false,"2549873769",
        undefined,c2s,c2s_shaper,false,false,false,false,
        [verify_none],
        false,undefined,[],"localhost",[],undefined,
        {0,nil},
        {0,nil},
        {0,nil},
        {0,nil},
        undefined,undefined,undefined,false,
        {userlist,none,[],false},
        unknown,unknown,
        {{127,0,0,1},64379},
        [],undefined,false,0,0,[],0,100,1},
       [60,63,120,109,108,32,118,101,114,115,105,111,110,61,39,49,46,
        48,39,63,62,60,115,116,114,101,97,109,58,115,116,114,101,97,
        109,32,120,109,108,110,115,61,39,106,97,98,98,101,114,58,99,
        108,105,101,110,116,39,32,120,109,108,110,115,58,115,116,114,
        101,97,109,61,39,104,116,116,112,58,47,47,101,116,104,101,114,
        120,46,106,97,98,98,101,114,46,111,114,103,47,115,116,114,101,
        97,109,115,39,32,105,100,61,39,"2549873769",39,32,102,114,111,
        109,61,39,"localhost",39,
        [" version='","1.0","'"],
        [" xml:lang='","en","'"],
        62]]}}.

tx_streamend_trace() ->
    {trace,'some_pid',call,
     {ejabberd_c2s,send_text,
      [{state,
        {socket_state,gen_tcp,'some_port','some_pid'},
        ejabberd_socket,'some_ref',false,"3714186711",
        {sasl_state,"jabber","localhost",[],
         'some_fun',
         'some_fun',
         'some_fun',cyrsasl_digest,
         {state,5,"4200324794","asd",[],
          'some_fun',
          'some_fun',
          ejabberd_auth_internal,"localhost","x3"}},
        c2s,c2s_shaper,false,false,false,false,
        [verify_none],
        true,
        {jid,"asd","localhost","x3","asd","localhost","x3"},
        "asd","localhost","x3",
        {{1385,998071,193003},'some_pid'},
        {2,
         {{"qwe","localhost",[]},
          {{"asd","localhost",[]},nil,nil},
          nil}},
        {2,
         {{"qwe","localhost",[]},
          {{"asd","localhost",[]},nil,nil},
          nil}},
        {2,
         {{"asd","localhost",[]},
          nil,
          {{"qwe","localhost",[]},nil,nil}}},
        {0,nil},
        {xmlelement,"presence",
         [{"xml:lang","en"}],
         [{xmlcdata,<<"\n">>},
          {xmlelement,"priority",[],[{xmlcdata,<<"5">>}]},
          {xmlcdata,<<"\n">>},
          {xmlelement,"c",
           [{"xmlns","http://jabber.org/protocol/caps"},
            {"node","http://psi-im.org/caps"},
            {"ver","caps-b75d8d2b25"},
            {"ext","ca cs ep-notify-2 html"}],
           []},
          {xmlcdata,<<"\n">>}]},
        undefined,
        {{2013,12,2},{15,27,51}},
        false,
        {userlist,none,[],false},
        c2s,ejabberd_auth_internal,
        {{127,0,0,1},64491},
        [],"en",false,0,0,[],0,100,1},
       "</stream:stream>"]}}.

routed_in_trace() ->
    {trace,'some_pid','receive',
     {route,
      {jid,"asd","localhost",[],"asd","localhost",[]},
      {jid,"asd","localhost","x3","asd","localhost","x3"},
      {xmlelement,"iq",
       [{"id","ac6aa"},{"type","result"}],
       [{xmlelement,"query",
         [{"xmlns","jabber:iq:roster"}],
         [{xmlelement,"item",
           [{"subscription","both"},
            {"name","qwe@localhost"},
            {"jid","qwe@localhost"}],
           []},
          {xmlelement,"item",
           [{"subscription","none"},
            {"name","self"},
            {"jid","asd@localhost"}],
           []}]}]}}}.

routed_out_trace() ->
    {trace,'some_pid',send,
     {route,
      {jid,"asd","localhost","x3","asd","localhost","x3"},
      {jid,"qwe","localhost","x3","qwe","localhost","x3"},
      {xmlelement,"message",
       [{"xml:lang","en"},
        {"type","chat"},
        {"to","qwe@localhost/x3"},
        {"id","ac99a"}],
       [{xmlcdata,<<"\n">>},
        {xmlelement,"body",[],[{xmlcdata,<<"zxc123">>}]},
        {xmlcdata,<<"\n">>},
        {xmlelement,"active",
         [{"xmlns",
           "http://jabber.org/protocol/chatstates"}],
         []},
        {xmlcdata,<<"\n">>}]}},
     'other_pid'}.
