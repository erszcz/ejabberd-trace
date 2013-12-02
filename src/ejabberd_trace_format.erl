-module(ejabberd_trace_format).

-export([stream/2]).

stream(end_of_trace, _Opts) ->
    true;
stream({trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamstart, _, _} = StreamStart}}, _Opts) ->
    io:format("~p~n", [exml:to_iolist(StreamStart)]);
stream({trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamelement, XMLElem}}}, _Opts) ->
    XMLElem2 = case element(1, XMLElem) of
                   xmlelement ->
                       setelement(1, XMLElem, xmlel);
                   xmlel ->
                       XMLElem
               end,
    io:format("~p~n", [exml:to_iolist(XMLElem2)]);
stream({trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamend, _} = StreamEnd}}, _Opts) ->
    io:format("~p~n", [exml:to_iolist(StreamEnd)]);
stream({trace, _Pid, call,
        {ejabberd_c2s, send_text, [_State, Msg]}}, _Opts) ->
    io:format("~p~n", [Msg]);
stream(_, _Opts) ->
    false.
