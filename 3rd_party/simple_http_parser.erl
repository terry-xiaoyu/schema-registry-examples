-module(simple_http_parser).

-export([ start/1
        , stop/0
        ]).

-export([ xor_decode/1
        , xor_encode/1
        ]).

-export([init/2]).

-define(PORT_FLOOR, 8000).
-define(PORT_CEILING, 8080).

%%===============================================
%% APIs
%%===============================================

-spec start(Path::string())
      -> {ok, inet:port_number()} | {error, Reason::term()}.
start(Path) ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(crypto),
    Dispatch = cowboy_router:compile([
        {'_', [{Path, ?MODULE, #{type => encode}}]}
    ]),
    start_http(Dispatch, ?PORT_FLOOR).

start_http(_Dispatch, ?PORT_CEILING) ->
    {error, {port_range_inuse, {?PORT_FLOOR, ?PORT_CEILING}}};
start_http(Dispatch, Port) ->
    stop(),
    case cowboy:start_clear(?MODULE,
            [{port, Port}],
            #{env => #{dispatch => Dispatch}}
         ) of
        {ok, _} ->
            logger:notice("===== started_listener ~p", [?MODULE]),
            {ok, Port};
        {error, eaddrinuse} ->
            NextPort = Port + 1,
            logger:notice("Port ~p is inuse, trying ~p...", [NextPort]),
            start_http(Dispatch, NextPort)
    end.

stop() ->
    logger:notice("===== stop_listener ~p", [?MODULE]),
    cowboy:stop_listener(?MODULE).

%%===============================================
%% Cowboy callbacks
%%===============================================

init(Req0, State) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Req = parser_req(Method, HasBody, State, Req0),
	{ok, Req, State}.

parser_req(<<"POST">>, true, #{type := decode}, Req0) ->
    {ok, RawData, Req} = get_payload(Req0),
	reply(xor_decode(base64:decode(RawData)), Req);
parser_req(<<"POST">>, true, #{type := encode}, Req0) ->
	{ok, Payload, Req} = get_payload(Req0),
	reply(xor_encode(base64:decode(Payload)), Req);
parser_req(<<"POST">>, false, _State, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
parser_req(_, _, _State, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

get_payload(Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
	#{<<"payload">> := Payload} = jsx:decode(Body, [return_maps]),
    {ok, Payload, Req}.

reply(Data, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"application/json; charset=utf-8">>
	}, result(1, Data), Req).

xor_encode(Data) ->
    xor_decode(Data).
xor_decode(Data) ->
    Secret = list_to_binary(lists:duplicate(byte_size(Data), "a")),
    crypto:exor(Data, Secret).

result(Code, Data) ->
    jsx:encode(#{
        code => Code,
        result => base64:encode(Data)
    }).

