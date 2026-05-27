# OCPP Version 2.0.1 Station & CSMS State Machine PropEr Test

This module implements a model of a charging station, its connection to
the CSMS (the process that calls `ocpp_station:connect/2`), and the
CSMS itself. The objective of this model is to test not only the happy
path of a well behaved station and connection, but also a misbehaving
connection.

## Model

The state of the model is described by a three-tuple `{AppState,
ConnectionState, StationState}`

* `AppState` is the modeled state of the CSMS. In other words, it is
  the state `ocpp_station` state machine thinks the station is in.
  Valid states are

  | `AppState`      | meaning                                                       |
  |-----------------|---------------------------------------------------------------|
  | `unprovisioned` | station has not been accepted                                 |
  | `booting`       | CSMS received `BootNotificationRequest`; has not responded    |
  | `pending`       | CSMS responded to `BootNotification` with `status="Pending"`  |
  | `accepted`      | CSMS responded to `BootNotification` with `status="Accepted"` |

  Valid state transitions are

  | From            | To              | Note                                               |
  |-----------------|-----------------|----------------------------------------------------|
  | `unprovisioned` | `booting`       | Received `BootNotificationRequest`                 |
  | `booting`       | `pending`       | Sent `BootNotificationResponse(status="Pending")`  |
  | `booting`       | `accepted`      | Sent `BootNotificationResponse(status="Accepted")` |
  | `booting`       | `unprovisioned` | Sent `BootNotificationResponse(status="Rejected")` |
  | `accepted`      | `booting`       | Received `BootNotificationRequest`                 |

* `ConnectionState` is a binary flag indicating whether or not the
  connection process is connected.

  | `ConnectionState` | meaning                                   |
  |-------------------|-------------------------------------------|
  | `connected`       | connection process alive and connected    |
  | `disconnected`    | connection process alive and disconnected |

  Transition table. Two alternate paths exist to reach `disconnected`.

  | From           | To             | Note                                              |
  |----------------|----------------|---------------------------------------------------|
  | `disconnected` | `connected`    | `ocpp_station:connect/2` returned `{ok, '2.0.1'}` |
  | `connected`    | `disconnected` | `ocpp_station:disconnect/1`                       |
  | `connected`    | `disconnected` | via `exit(ConnPid)`                               |

* `StationState` models the physical state of the station. It
  consists of two states (`up` and `down`) indicating if the station
  is powered on and includes an extra flag with the `up` state that
  indicates whether the station is up after a reboot and is required
  to go through the provisioning process again.

  | `StationState` | meaning                                         |
  |----------------|-------------------------------------------------|
  | `down`         | Station is powered down                         |
  | `clean`  | Station is up & CSMS state is known to be clean |
  | `dirty`  | Station is up & CSMS state may be dirty         |

  The transition table helps to clarify what the `clean` and `dirty`
  flags mean.

  | From          | To            | Note                                     |
  |---------------|---------------|------------------------------------------|
  | `down`        | `{up, clean}` | initial boot                             |
  | `down`        | `{up, dirty}` | reboot                                   |
  | `{up, clean}` | `down`        | shutdown                                 |
  | `{up, dirty}` | `{up, clean}` | station sent a `BootNotificationRequest` |

  Not enough information exists in the `StationState` to identify what
  type transition applies from the `down` state; however, by combining
  this state with the `AppState` it is possible to infer whether a
  reboot or an initial boot is occuring. (`AppState` will be
  `unprovisioned` on initial boot).

All three of these combine to form a large state table (4 × 2 × 3 = 24
possible states); however not all states are reachable. The full
transition table, excluding self-transitions, is below.

| From            |                   |                | To              |                   |                |         |         |              |               |       |
| `AppState`      | `ConnectionState` | `StationState` | `AppState`      | `ConnectionState` | `StationState` | Note    | Command | Precondition | Postcondition | State |
|-----------------|-------------------|----------------|-----------------|-------------------|----------------|---------|---------|--------------|---------------|-------|
| `unprovisioned` | `disconnected`    | `down`         | `unprovisioned` | `disconnected`    | `{up, clean}`  | [1]     | ✅      | ✅           | ✅            | ⇔     |
| `unprovisioned` | `disconnected`    | `{up, clean}`  | `unprovisioned` | `connected`       | `{up, clean}`  |         | ✅      | ✅           | ✅            | ⇔     |
| `unprovisioned` | `connected`       | `{up, clean}`  | `unprovisioned` | `disconnected`    | `{up, clean}`  |         | ✅      | ✅           | ✅            | ⇔     |
| `unprovisioned` | `connected`       | `_`            | `booting`       | `connected`       | `{up, clean}`  |         | ✅      | ✅           | ✅            | ✅    |
|-----------------|-------------------|----------------|-----------------|-------------------|----------------|---------|---------|--------------|---------------|-------|
| `booting`       | `connected`       | `{up, Flag}`   | `booting`       | `disconnected`    | `{up, Flag}`   |         |         |              |               |       |
| `booting`       | `disconnected`    | `{up, Flag}`   | `booting`       | `connected`       | `{up, Flag}`   |         |         |              |               |       |
| `booting`       | `connected`       | `{up, clean}`  | `pending`       | `connected`       | `{up, clean}`  |         |         |              |               |       |
| `booting`       | `connected`       | `{up, clean}`  | `accepted`      | `connected`       | `{up, clean}`  |         |         |              |               |       |
| `booting`       | `connected`       | `{up, clean}`  | `unprovisioned` | `connected`       | `{up, clean}`  | [5]     |         |              |               |       |
| `booting`       | `connected`       | `{up, dirty}`  | `booting`       | `connected`       | `{up, clean}`  | [4]     |         |              |               |       |
| `booting`       | `connected`       | `{up, dirty}`  | `unprovisioned` | `connected`       | `{up, dirty}`  | [6]     |         |              |               |       |
| `booting`       | `connected`       | `{up, dirty}`  | `pending`       | `connected`       | `{up, dirty}`  | [6]     |         |              |               |       |
| `booting`       | `connected`       | `{up, dirty}`  | `accepted`      | `connected`       | `{up, dirty}`  | [6]     |         |              |               |       |
| `booting`       | `disconnected`    | `{up, _}`      | `booting`       | `disconnected`    | `down`         | [2]     |         |              |               |       |
| `booting`       | `disconnected`    | `down`         | `booting`       | `disconnected`    | `{up, dirty}`  | [1],[3] |         |              |               |       |
|-----------------|-------------------|----------------|-----------------|-------------------|----------------|---------|---------|--------------|---------------|-------|
| `accepted`      | `connected`       | `{up, Flag}`   | `accepted`      | `disconnected`    | `{up, Flag}`   |         |         |              |               |       |
| `accepted`      | `disconnected`    | `{up, Flag}`   | `accepted`      | `connected`       | `{up, Flag}`   |         |         |              |               |       |
| `accepted`      | `disconnected`    | `{up, _}`      | `accepted`      | `disconnected`    | `down`         | [2]     |         |              |               |       |
| `accepted`      | `disconnected`    | `down`         | `accepted`      | `disconnected`    | `{up, dirty}`  | [1],[3] |         |              |               |       |
| `accepted`      | `connected`       | `_`            | `booting`       | `connected`       | `{up, clean}`  | [4]     |         |              |               |       |
|-----------------|-------------------|----------------|-----------------|-------------------|----------------|---------|---------|--------------|---------------|-------|
| `pending`       | `connected`       | `{up, Flag}`   | `pending`       | `disconnected`    | `{up, Flag}`   |         |         |              |               |       |
| `pending`       | `disconnected`    | `{up, Flag}`   | `pending`       | `connected`       | `{up, Flag}`   |         |         |              |               |       |
| `pending`       | `disconnected`    | `{up, _}`      | `pending`       | `disconnected`    | `down`         | [2]     |         |              |               |       |
| `pending`       | `disconnected`    | `down`         | `pending`       | `disconnected`    | `{up, dirty}`  | [1],[3] |         |              |               |       |
| `pending`       | `connected`       | `_`            | `booting`       | `connected`       | `{up, clean}`  | [4]     |         |              |               |       |
|-----------------|-------------------|----------------|-----------------|-------------------|----------------|---------|---------|--------------|---------------|-------|
| `unprovisioned` | `disconnected`    | `{up, clean}`  | `unprovisioned` | `disconnected`    | `down`         | [1]     | trivial | don't test   | pointless     | 🛑    |



[1]: trivial transition; no data change or interaction with `ocpp_station`
[2]: station side state is cleared in the model
[3]: `dirty` is inferred because `AppState =/= unprovisioned`
[4]: Station sends a `BootNotificationRequest`, effectively informing the CSMS it has rebooted and enabling it to clear its state.
[5]: Station rejected
[6]: CSMS responds to pending request after a disconnect/reconnect cycle and unknown (to the CSMS) station reboot

Each transition in this table is the result of a succesful
`ocpp_station:rpc/2`, `ocpp_station:reply/3`,
`ocpp_station:connect/2`, or a trivial tansition ([1]). For the
non-trivial cases the success postconditions are as follows.

| Command                  | Result          | Assertion                            |
|--------------------------|-----------------|--------------------------------------|
| `ocpp_station:connect/2` | `{ok, '2.0.1'}` |                                      |
| `ocpp_station:rpc/2`     | `ok`            |                                      |
| `ocpp_station:reply/3`   | `ok`            | Connection proc. received CALLRESULT |

Self-transitions exhibit a considerably larger and more complex set of
behaviors. **Note** in the table below only self-transitions are
described. Where a command could produce a transition to a different
state depending on its contents (for example, `csms_reply` could send
a `BootNotificationResponse` in the `booting` state) it is assumed
that such payloads are excluded from the command in the given state.

| `AppState`      | `ConnectionState` | `StationState` | Command                     | Command | Precondition | Result                       | Assertion   | Postcondition | State |
|-----------------|-------------------|----------------|-----------------------------|---------|--------------|------------------------------|-------------|---------------|-------|
| `unprovisioned` | `disconnected`    |                | `csms_call`                 | ✅      | ✅           | `{error, not_connected}`     | [NORPCSEND] | ✅            | ⇔    |
| `unprovisioned` | `disconnected`    |                | `csms_reply`                | ✅      | ✅           | `{error, not_connected}`     | [NORPCSEND] | ✅            | ⇔    |
| `unprovisioned` | `disconnected`    |                | `station_call`              | ✅      | ✅           | `{error, not_connected}`     |             | ✅            | ⇔    |
| `unprovisioned` | `disconnected`    |                | `station_reply`             | ✅      | ✅           | `{error, not_connected}`     |             | ✅            | ⇔    |
| `unprovisioned` | `disconnected`    |                | `connect_unsupported`       | ✅      | ✅           | `{error, not_supported}`     |             | ✅            | ⇔    |
| `unprovisioned` | `connected`       |                | `csms_call`                 | ✅      | ✅           | `{error, not_provisioned}`   | [NORPCSEND] | ✅            | ⇔    |
| `unprovisioned` | `connected`       |                | `csms_reply`                | ✅      | ✅           | `{error, not_provisioned}`   | [NORPCSEND] | ✅            | ⇔    |
| `unprovisioned` | `connected`       |                | `station_call`              | ✅      | ✅           | `ok`                         | [SECERROR]  | ✅            | ⇔    |
| `unprovisioned` | `connected`       |                | `station_reply`             | ✅      | ✅           | `ok`                         |             | ✅            | ⇔    |
| `unprovisioned` | `connected`       |                | `connect_upsupported`       | ✅      | ✅           | `{error, already_connected}` |             | ✅            | ⇔    |
| `unprovisioned` | `connected`       |                | `connect_supported`         | ✅      | ✅           | `{error, already_connected}` |             | ✅            | ⇔    |
|-----------------|-------------------|----------------|-----------------------------|---------|--------------|------------------------------|-------------|---------------|-------|
| `booting`       | `disconnected`    |                | `csms_call`                 |         |              | `{error, not_connected}`     | [NORPCSEND] |               |       |
| `booting`       | `disconnected`    |                | `csms_reply`                |         |              | `{error, not_connected}`     | [NORPCSEND] |               |       |
| `booting`       | `disconnected`    |                | `station_call`              |         |              | `{error, not_connected}`     |             |               |       |
| `booting`       | `disconnected`    |                | `station_reply`             |         |              | `{error, not_connected}`     |             |               |       |
| `booting`       | `connected`       |                | `csms_call`                 |         |              | `{error, not_provisioned}`   | [NORPCSEND] |               |       |
| `booting`       | `connected`       | `{up, dirty}`  | `csms_reply`                |         |              | `{error, call_not_pending}`  | [NORPCSEND] |               |       |
| `booting`       | `connected`       | `{up, clean}`  | `csms_reply`                |         |              | `{error, call_not_pending}`  | [NORPCSEND] |               |       |
| `booting`       | `connected`       |                | `station_call`              |         |              | `ok`                         | [SECERROR]  |               |       |
| `booting`       | `connected`       |                | `station_reply`             |         |              | `ok`                         |             |               |       |
|-----------------|-------------------|----------------|-----------------------------|---------|--------------|------------------------------|-------------|---------------|-------|
| `pending`       | `disconnected`    |                | `csms_call`                 |         |              | `{error, not_connected}`     | [NORPCSEND] |               |       |
| `pending`       | `disconnected`    |                | `csms_reply`                |         |              | `{error, not_connected}`     | [NORPCSEND] |               |       |
| `pending`       | `disconnected`    |                | `station_call`              |         |              | `{error, not_connected}`     |             |               |       |
| `pending`       | `disconnected`    |                | `station_reply`             |         |              | `{error, not_connected}`     |             |               |       |
| `pending`       |                   |                | `csms_call_cip`             |         |              | `{error, {cip, _}}`          | [NORPCSEND] |               |       |
| `pending`       | `connected`       |                | `csms_call`                 |         |              | `{error, illegal_call}`      | [NORPCSEND] |               |       |
| `pending`       | `connected`       |                | `csms_call_config`          |         |              | `ok`                         | [RPCCALL]   |               |       |
| `pending`       | `connected`       |                | `csms_reply`                |         |              | `ok`                         | [RPCREPLY]  |               |       |
| `pending`       | `connected`       |                | `csms_reply`                |         |              | `{error, call_not_pending}`  | [NORPCSEND] |               |       |
| `pending`       | `connected`       |                | `station_reply`             |         |              | `ok`                         |             |               |       |
| `pending`       | `connected`       |                | `station_call`              |         |              | `ok`                         | [SECERROR]  |               |       |
| `pending`       | `connected`       |                | `station_call_config`       |         |              | `ok`                         | [NORPCSEND] |               |       |
| `pending`       | `connected`       | `{up, clean}`  | `station_call_stale_config` |         |              | `ok`                         | [SECERROR]  |               |       |
| `pending`       | `connected`       | `{up, dirty}`  | `station_call_stale_config` |         |              | `ok`                         | [NORPCSEND] |               |       |
|-----------------|-------------------|----------------|-----------------------------|---------|--------------|------------------------------|-------------|---------------|-------|
| `accepted`      | `disconnected`    |                | `csms_call`                 |         |              | `{error, not_connected}`     | [NORPCSEND] |               |       |
| `accepted`      | `disconnected`    |                | `csms_reply`                |         |              | `{error, not_connected}`     | [NORPCSEND] |               |       |
| `accepted`      | `disconnected`    |                | `station_call`              |         |              | `{error, not_connected}`     |             |               |       |
| `accepted`      | `disconnected`    |                | `station_reply`             |         |              | `{error, not_connected}`     |             |               |       |
| `accepted`      |                   |                | `csms_call_cip`             |         |              | `{error, {cip, _}}`          | [NORPCSEND] |               |       |
| `accepted`      | `connected`       |                | `csms_call`                 |         |              | `ok`                         | [RPCCALL]   |               |       |
| `accepted`      | `connected`       |                | `csms_reply`                |         |              | `ok`                         | [RPCREPLY]  |               |       |
| `accepted`      | `connected`       |                | `csms_reply`                |         |              | `{error, call_not_pending}`  | [NORPCSEND] |               |       |
| `accepted`      | `connected`       |                | `station_reply`             |         |              | `ok`                         |             |               |       |
| `accepted`      | `connected`       |                | `station_call`              |         |              | `ok`                         | [SECERROR]  |               |       |
|-----------------|-------------------|----------------|-----------------------------|---------|--------------|------------------------------|-------------|---------------|-------|
|                 | `connected`       |                | `connect_supported`         |         | ✅           | `{error, already_connected}` |             | ✅            |       |
|                 | `connected`       |                | `connect_unsuported`        |         | ✅           | `{error, already_connected}` |             | ✅            |       |

[NORPCSEND]: Verify an `{ocpp, {rpcsend, _}}` message does not arrive
[RPCCALL]: A `{ocpp, {rpcsend, Message}}` message is sent to the connection process; `Message` is a CALL with matching ID
[RPCREPLY]: A `{ocpp, {rpcsend, Message}}` message is sent to the connection process; `Message` is a CALLRESULT with matching ID
[SECERROR]: A `{ocpp, {rpcsend, Message}}` message is sent to the connection process; `Message` is a CALLERROR with type `SecurityError` and a matching ID

* `station_call_config` station sends an expected config message (e.g. triggered message or report message).
* `station_call_stale_config` send a stale configuration message that was present before the station rebooted.
