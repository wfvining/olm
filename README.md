# OCPP

An OTP application for communication with electric vehicle charging
stations using the Open Charge Point Protocol.

Version 1.0 of the application is a complete rewrite of the version
0.1 prototype. Lessons learned from the first version have been taken
where appropriate and many improvements are added. The most notable
changes are listed below.

## Changes from Version 0.1

### Enable Dialyzer to Check Message Construction

A major frustration with the prototype version was that message
construction was validated strictly at runtime. This lead to
inscrutable error messages from the `jerk` application and irritating
debugging loops while trying to figure out what parameter you passed
incorrectly. In version 1.0 we provide messages construction functions
for each message type along with type definitions for the messages
themselves and the maps used to construct the messages. Using these
constructors it is possible for Dialyzer to check the maps provided
for the correct types and required keys prior to run time. Of course,
run time checking is still required for value constraints; however,
error messages from bad values have been improved.

The message construction modules are generated automatically from the
JSON schema files using `gen_messages.erl`. This can be built as an
escript using `rebar3 escriptize`. The output modules should not be
edited except to format them with `rebar3 fmt`. Changes must be made
to the generator and the generator re-run.

### Support Multiple OCPP Versions

In addition to message modules for each OCPP version there are several
architectural and runtime changes. To support multiple versions the
`ocpp_handler` callback module must support a call operation to tell
the station state machine what version to use when a new connection is
established. Besides this, changes to support multiple versions should
be mostly transparent to the user with the exception of message
construction using the `ocpp_message_1_6`, `ocpp_message_2_0_1`, or
`ocpp_message_2_1` modules.

### Additional Handlers

The station state machine is split into multiple state machines. The
principal machine, `ocpp_station`, is used to enforce state-based
constraints on which messages may be sent or received. Managing the
state of individual EVSE, transactions, or report requests is
delegated to separate state machines and event handlers. This division
provides an opportunity for users to install additional handlers for
transaction, reporting, or monitoring events.

Users may also install additional message handlers to enable
separation of business logic, logging, and diagnostics functionality
in a CSMS.

### Device Model

The device model can now be seeded directly when the station is added
to the application. While the details are not fully fleshed out, this
will make it easier to migrate the station to a different node or to
recover after a failure.

### Restarting `ocpp_station`

One of the issues with version 0.1 is that it depended on the station
sending a `BootNotificationRequest` when it connected after the
`ocpp_station` state machine was started (or restarted). This is not
actually the behavior required by the standard. In fact we cannot even
trigger a boot notification manually in this case because that too is
forbidden except in the pending state. The only option for forcing a
`BootNotificaionRequest` is to reset the station, forcing it to
reboot. That is not acceptable. By providing some extra state when it
starts we can inform a re-started `ocpp_station` state machine that
the station has already been accepted and it should not expect a
`BootNotificaitonRequest`. This is still being worked out in detail
and will likely include a fallback that forces a reset of the station
if the `ocpp_station` process cannot successfully recover from the
persistent state.
