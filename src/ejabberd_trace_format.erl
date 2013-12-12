-module(ejabberd_trace_format).

-export([raw/2,
         stream/2,
         file/1]).

raw(Trace, _Opts) ->
    io:format("raw: ~p~n", [Trace]).

stream({trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamstart, _, _} = StreamStart}}, _Opts) ->
    io:format("in :~n~ts~n", [to_iolist(StreamStart)]);
stream({trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamelement, Elem}}}, _Opts) ->
    io:format("in :~n~ts~n", [to_iolist(xmlelement_to_xmlel(Elem))]);
stream({trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamend, _} = StreamEnd}}, _Opts) ->
    io:format("in :~n~ts~n", [to_iolist(StreamEnd)]);
stream({trace, _Pid, call,
        {ejabberd_c2s, send_text, [_State, Msg]}}, _Opts) ->
    io:format("out :~n~ts~n~n", [Msg]);
stream({trace, _Pid, call,
        {ejabberd_c2s, send_element, [_State, Elem]}}, _Opts) ->
    io:format("out :~n~ts~n", [to_iolist(xmlelement_to_xmlel(Elem))]);
stream(_, _Opts) ->
    ok.

file(Filepath) ->
    {ok,File}=file:open(Filepath,[write]),
    fun(Trace,Opts) ->
        file(File,Trace,Opts)
    end.

file(File,{trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamstart, _, _} = StreamStart}}, _Opts) ->
    file:write(File,io_lib:format("in :~n~ts~n", [to_iolist(StreamStart)]));
file(File,{trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamelement, Elem}}}, _Opts) ->
    file:write(File,io_lib:format("in :~n~ts~n", [to_iolist(xmlelement_to_xmlel(Elem))]));
file(File,{trace, _Pid, 'receive',
        {'$gen_event', {xmlstreamend, _} = StreamEnd}}, _Opts) ->
    file:write(File,io_lib:format("in :~n~ts~n", [to_iolist(StreamEnd)]));
file(File,{trace, _Pid, call,
        {ejabberd_c2s, send_text, [_State, Msg]}}, _Opts) ->
    file:write(File,io_lib:format("out :~n~ts~n~n", [Msg]));
file(File,{trace, _Pid, call,
        {ejabberd_c2s, send_element, [_State, Elem]}}, _Opts) ->
    file:write(File,io_lib:format("out :~n~ts~n", [to_iolist(xmlelement_to_xmlel(Elem))]));
file(_File,_, _Opts) ->
    ok.

xmlelement_to_xmlel({xmlelement, Name, Attrs, Children}) ->
    {xmlel, Name, Attrs, [xmlelement_to_xmlel(C) || C <- Children]};
xmlelement_to_xmlel(Other) ->
    Other.

to_iolist(Elem) ->
    exml:to_pretty_iolist(Elem).
