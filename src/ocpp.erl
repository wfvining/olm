-module(ocpp).

-export([version_to_protocol/1, protocol_to_version/1]).

-export_type([version/0]).

-type version() :: '1.6' | '2.0.1' | '2.1'.

version_to_protocol(Version) ->
    Vsn = atom_to_binary(Version),
    <<"ocpp", Vsn/binary>>.

protocol_to_version(<<"ocpp", Vsn/binary>>) ->
    binary_to_existing_atom(Vsn).
