# ejabberd-trace

This is an extremely simple utility for easy tracing of connections made
to ejabberd XMPP server.

In the future it might be expanded but just as well it might be abandoned
due to proving to be useless.

The project is in rapid development - you have been warned.
(Read: I don't care for API backwards compatibility for now).

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

### Display incoming/outgoing XMPP streams for a given user

This gives a complete pretty-printed log of the whole client
to server connection for a single full JID:

    > ejabberd_trace:new_user("alice@localhost/escalus-default-resource",
                              ejabberd_trace_filter:stream(),
                              fun ejabberd_trace_format:stream/2).

Example output:

    in :
    <stream:stream to='localhost' version='1.0' xml:lang='en' xmlns='jabber:client'
                   xmlns:stream='http://etherx.jabber.org/streams'>

    out :
    <?xml version='1.0'?>
    <stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'
                   id='1217298371' from='localhost' version='1.0' xml:lang='en'>

    out :
    <stream:features>
      <mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>
        <mechanism>DIGEST-MD5</mechanism>
        <mechanism>PLAIN</mechanism>
        <mechanism>SCRAM-SHA-1</mechanism>
      </mechanisms>
      <c ver='mfN6SdQ3DGO7/QUHHftElVDFZ7k='
         node='http://www.process-one.net/en/ejabberd/'
         hash='sha-1'
         xmlns='http://jabber.org/protocol/caps'/>
      <register xmlns='http://jabber.org/features/iq-register'/>
      <sm xmlns='urn:xmpp:sm:3'/>
    </stream:features>

    in :
    <auth mechanism='PLAIN' xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>AGFsaWNlAG1ha290YQ==</auth>

    out :
    <success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>

    ...

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

- [x] Implement `routed_out` filter.

- [x] Implement `routed_in` filter.

- [x] Make filter and formatter choices independent.
      Filter should only filter traces and pass them to a formatter.

- [x] Make filters composable.
      It should be possible to display `routed_in` and `tx` traces
      simultaneously.

      There are two combinators available: `any` and `all`.

- [x] Sort out logging.
      There are debug messages logged unconditionally right now, e.g.:

        >>>>> caught send_element
        >>>>> found no Jid

      It would be nice to keep them, but not display all the time.
      It should also be possible to print the traces to a file.

      These are only compiled in when the `debug` compile flag is set.

- [ ] Trace multiple subsequent connections of a given user.
      A test story may involve the same full jid connecting more than
      once to fulfill one scenario.

- [ ] Enable functioning in a distributed environment.

      This seems to work. Sometimes.

- [ ] In case of BOSH users the trigger might happen on two distinct
      events: the call in c2s process or sending the message from BOSH
      socket. Find a way to discriminate one (store opts in trace server
      ets? and verify in do_trace_user?).
