-module(ejabberd_trace_format).

-export([stream/2]).

stream({trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamstart, _, _} = StreamStart}}, _Opts) ->
    io:format("in :~n~ts~n~n", [to_iolist(StreamStart)]);
stream({trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamelement, Elem}}}, _Opts) ->
    io:format("in :~n~ts~n~n", [to_iolist(xmlelement_to_xmlel(Elem))]);
stream({trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamend, _} = StreamEnd}}, _Opts) ->
    io:format("in :~n~ts~n~n", [to_iolist(StreamEnd)]);
stream({trace, _Pid, call,
        {ejabberd_c2s, send_text, [_State, Msg]}}, _Opts) ->
    io:format("out :~n~ts~n~n", [Msg]);
stream(_, _Opts) ->
    ok.

xmlelement_to_xmlel({xmlelement, Name, Attrs, Children}) ->
    {xmlel, Name, Attrs, [xmlelement_to_xmlel(C) || C <- Children]};
xmlelement_to_xmlel(Other) ->
    Other.

to_iolist(Elem) ->
    exml:to_pretty_iolist(Elem).
