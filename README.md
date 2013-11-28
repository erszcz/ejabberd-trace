# ejabberd-trace

This is an extremely simple utility for easy tracing of connections made
to ejabberd XMPP server.

In the future it might be expanded but just as well it might be abandoned
due to proving to be useless.

## Getting started

### Tracing a connected user by JID

Trace a user who is connected to an ejabberd node:

    > ejabberd_trace:user("asd@localhost/a-resource").
    > % or
    > ejabberd_trace:user("asd@localhost").

If you don't specify a resource and there are multiple available:

    > ejabberd_trace:user("asd@localhost").
    {error,{multiple_sessions,[{{"asd","localhost","psi"},
                                <0.327.0>},
                               {{"asd","localhost","x3"},<0.307.0>}]}}
    > ejabberd_trace:user("asd@localhost/x3").

### Tracing a user that is going to connect soon by JID

It's possible to trace a user who is not connected yet
but will connect in a short period of time using only the JID.

It's useful in case you need to trace all communication
(including connection initiation) happening between a c2s process
and other parts of the system (including TCP connection).

    > ejabberd_trace:new_user("asd@localhost").

Keep in mind **this might be heavy** on the system - until it's known which
of the connected processes is the one you want to trace **all newly connecting
c2s processes are traced and the traces cached**.
Once it's known (by inspecting the XMPP stream) which process is the one
to be traced its trace cache is flushed to the trace handler; the rest of
the trace cache is discarded.

### Display c2s process state by JID

    > ejabberd_trace:state("asd@localhost").

## Filters

It's possible to filter the traces caught for the traced process.
The available filters are:

- `raw_traces` - all raw trace messages
- `dbg` - all trace messages as formatted by dbg
- `tx` - messages sent on socket as formatted by dbg
- `rx` - messages received on socket as formatted by dbg
- `routed_out` - messages routed to the c2s process from other parts
                 of the server
- `routed_in` - messages routed by the c2s process to other part of the
                server

## ToDo

- [ ] Implement `routed_out` filter.

- [x] Implement `routed_in` filter.

- [ ] Make filter and formatter choices independent.
      Filter should only filter traces and pass them to a formatter.

- [ ] Make filters composable.
      It should be possible to display `routed_in` and `tx` traces
      simultaneously.

- [ ] Sort out logging.
      There are debug messages logged unconditionally right now, e.g.:

        >>>>> caught send_element
        >>>>> found no Jid

      It would be nice to keep them, but not display all the time.
      It should also be possible to print the traces to a file.
