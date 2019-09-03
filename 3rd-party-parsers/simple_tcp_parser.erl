-module(simple_tcp_parser).

-export([ start/1
        , accept/1
        , do_receive/1
        , stop/0
        ]).

-define(PORT_FLOOR, 9000).
-define(PORT_CEILING, 9080).

-define(LSOCK, {?MODULE, listen_sock}).

-record(parse_request, {parse_type,
                        msg_id,
                        schema_id,
                        parser_opts,
                        content
                        }).

-record(parse_result, {parse_type,
                       code,
                       msg_id,
                       content
                       }).

%%===============================================
%% APIs
%%===============================================

-spec start(Host::string())
      -> {ok, inet:port_number()} | {error, Reason::term()}.
start(Host) ->
    application:ensure_all_started(crypto),
    {ok, Host0} = inet:parse_address(Host),
    start_tcp(Host0, ?PORT_FLOOR).

start_tcp(_Dispatch, ?PORT_CEILING) ->
    {error, {port_range_inuse, {?PORT_FLOOR, ?PORT_CEILING}}};
start_tcp(Host, Port) ->
    stop(),
    case gen_tcp:listen(Port, [{active,true}, {packet,4}, {ip,Host}, binary]) of
        {ok, LSock} ->
            erlang:put(?LSOCK, LSock),
            spawn_link(fun() -> accept(LSock) end),
            {ok, Port};
        {error, eaddrinuse} ->
            NextPort = Port + 1,
            logger:notice("Port ~p is inuse, trying ~p...", [NextPort]),
            start_tcp(Host, NextPort)
    end.

accept(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            Worker = spawn_link(fun() -> do_receive(Sock) end),
            gen_tcp:controlling_process(Sock, Worker),
            accept(LSock);
        {error, Reason} ->
            error({accept_error, Reason})
    end.

do_receive(Sock) ->
    receive
        {tcp, Sock, Data} ->
            logger:notice("[~p] received: ~p", [?MODULE, Data]),
            {ok, ParseReq = #parse_request{content=Content}} =
                emqx_schema_tcp:unpack_parse_request(Data),
            logger:notice("[~p] unpacked: ~p", [?MODULE, ParseReq]),

            Result = #parse_result{parse_type = ParseReq#parse_request.parse_type,
                                   msg_id = ParseReq#parse_request.msg_id,
                                   code = 1, content = xor_encode(Content)},
            ParseResp = emqx_schema_tcp:pack_parse_result(Result),
            logger:notice("[~p] reply: ~p", [?MODULE, ParseResp]),
            gen_tcp:send(Sock, ParseResp),
            do_receive(Sock);
        Msg ->
            logger:notice("[~p] received: ~p, stop", [?MODULE, Msg])
    end.

stop() ->
    case erlang:get(?LSOCK) of
        undefined -> ok;
        Sock ->
            gen_tcp:close(Sock)
    end.

xor_encode(Data) ->
    xor_decode(Data).
xor_decode(Data) ->
    Secret = list_to_binary(lists:duplicate(byte_size(Data), "a")),
    crypto:exor(Data, Secret).
