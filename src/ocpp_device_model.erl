-module(ocpp_device_model).

-behavior(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(registry(Name), {via, gproc, ?name(Name)}).
-define(name(Name), {n, l, {ocpp_station, Name, device_model}}).

-export_type([variable/1]).

-type component() :: #{
    name := binary(),
    evse => pos_integer(),
    connector => pos_integer(),
    instance => binary()
}.

-type variableid() :: #{name := binary(), instance => binary()}.

%% -type attribute(Type) :: #{
%%                            type := attribute_type(),
%%                            value => Type,
%%                            mutability => mutability(),
%%                            persistent => boolean(),
%%                            constant => boolean()
%%                           }.

-type attribute_type() :: actual | target | minset | maxset.

-type mutability() :: [read | write].

-type characteristics() :: #{
    unit => binary(),
    type := data_type(),
    minlimit => number(),
    maxlimit => number(),
    values => [binary()],
    supports_monitoring := boolean()
}.

-type data_type() ::
    string
    | decimal
    | integer
    | datetime
    | boolean
    | optionlist
    | sequencelist
    | memberlist.

-type variable(Type) :: {
    {component(), variableid(), attribute_type()},
    Type | undefined,
    characteristics(),
    mutability(),
    %% persistent
    boolean(),
    %% constant
    boolean()
}.

-record(state, {variables :: ets:table()}).

-doc """
Start a new device model server.

The parameter `DeviceModel` can be used to initialize the new device
model from an existing device model. This is useful if you have a
previous version of the station device model and want to initialize
with the known values.
""".
-spec start_link(StationID :: binary(), [variable(any())]) ->
    gen_server:start_ret().
start_link(StationID, DeviceModel) ->
    gen_server:start_link(?registry(StationID), ?MODULE, DeviceModel, []).

init(DeviceModel) ->
    Table = ets:new(ocpp_device_model, [set, protected]),
    ets:insert(Table, DeviceModel),
    {ok, #state{variables = Table}}.

handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.
