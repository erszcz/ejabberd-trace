-module(ejabberd_trace_filter).

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
-export([apply/2]).

%% Types
-type filter() :: (predefined_filter() |
                   {fn, filter_fun()} |
                   {any, [filter()]} |
                   {all, [filter()]}).
-export_type([filter/0]).

-type predefined_filter() :: all | rx | tx | routed_in | routed_out | stream.
-export_type([predefined_filter/0]).

-define(FILTER_FUN_DEF, (trace()) -> boolean()).
-type filter_fun() :: fun(?FILTER_FUN_DEF).
-export_type([filter_fun/0]).

-type trace() :: any().
-export_type([trace/0]).

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

%%
%% Tests
%%

-ifdef(TEST).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-define(_eq(E, I), ?_assertEqual(E, I)).

rx_test_() ->
    [?_eq(true, rx(rx_trace())),
     ?_eq(false, rx(tx_text_trace())),
     ?_eq(false, rx(routed_in_trace())),
     ?_eq(false, rx(routed_out_trace()))].

tx_test_() ->
    [?_eq(true, tx(tx_element_trace())),
     ?_eq(true, tx(tx_streamstart_trace())),
     ?_eq(true, tx(tx_streamend_trace())),
     ?_eq(false, tx(tx_text_trace())),
     ?_eq(false, tx(rx_trace())),
     ?_eq(false, tx(routed_in_trace())),
     ?_eq(false, tx(routed_out_trace()))].

tx_text_test_() ->
    [?_eq(true, tx_text(tx_text_trace())),
     ?_eq(false, tx_text(rx_trace())),
     ?_eq(false, tx_text(routed_in_trace())),
     ?_eq(false, tx_text(routed_out_trace()))].

tx_element_test_() ->
    [?_eq(true, tx_element(tx_element_trace())),
     ?_eq(false, tx_element(tx_text_trace())),
     ?_eq(false, tx_element(rx_trace())),
     ?_eq(false, tx_element(routed_in_trace())),
     ?_eq(false, tx_element(routed_out_trace()))].

routed_in_test_() ->
    RI = fun routed_in/1,
    [?_eq(true, RI(routed_in_trace())),
     ?_eq(false, RI(routed_out_trace())),
     ?_eq(false, RI(rx_trace())),
     ?_eq(false, RI(tx_text_trace()))].

routed_out_test_() ->
    RO = fun routed_out/1,
    [?_eq(true, RO(routed_out_trace())),
     ?_eq(false, RO(routed_in_trace())),
     ?_eq(false, RO(rx_trace())),
     ?_eq(false, RO(tx_text_trace()))].

stream_test_() ->
    S = fun stream/1,
    [?_eq(true, S(tx_streamstart_trace())),
     ?_eq(true, S(tx_streamend_trace())),
     ?_eq(true, S(tx_element_trace())),
     ?_eq(true, S(rx_trace())),
     ?_eq(false, S(routed_in_trace())),
     ?_eq(false, S(routed_out_trace()))].

apply_test_() ->
    Ap = fun ?MODULE:apply/2,
    [?_eq(true, Ap(rx, rx_trace())),
     ?_eq(false, Ap(rx, tx_text_trace())),
     ?_eq(true, Ap({any, [rx, tx]}, rx_trace())),
     ?_eq(false, Ap({all, [rx, tx]}, rx_trace())),
     ?_eq(true, Ap({fn, fun ?MODULE:rx/1}, rx_trace()))].

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

-endif.
