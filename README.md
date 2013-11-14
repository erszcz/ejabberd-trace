# ejabberd-trace

This is an extremely simple utility for easy tracing of connections made
to ejabberd XMPP server.

In the future it might be expanded but just as well it might be abandoned
due to proving to be useless.

## Getting started

### Tracing a connected user

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
