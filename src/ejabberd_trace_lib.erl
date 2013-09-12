-module(ejabberd_trace_lib).

-export([get_env/3,
         extract_jid/1 ]).

-include("ejabberd_trace_internal.hrl").

%% @doc Return `Application' environment variable `Par'.
%% If no such variable is defined return `Def'.
%% This is modelled after `application:get_env/3' from R16B01.
get_env(Application, Par, Def) ->
    case application:get_env(Application, Par) of
        undefined ->
            Def;
        {ok, Val} ->
            Val
    end.

-define(IS_IQ(XML, IQ),
        XML =:= xmlel orelse XML =:= xmlelement,
        IQ =:= "iq" orelse IQ =:= <<"iq">>).

-define(IS_BIND(XML, Bind),
        XML =:= xmlel orelse XML =:= xmlelement,
        Bind =:= "bind" orelse Bind =:= <<"bind">>).

-spec extract_jid(ejt_xmlelement()) -> ejt_jid() | false.
extract_jid({XML, IQ, _, [{XML2, Bind, _, [JIDEl]}]} = IQ)
  when ?IS_IQ(XML, IQ) andalso ?IS_BIND(XML2, Bind) ->
    {_, _, _, {xmlcdata, JID}} = JIDEl,
    JID;
extract_jid(_) ->
    false.
