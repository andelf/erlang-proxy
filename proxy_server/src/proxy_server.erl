-module(proxy_server).

-export([start/0]).

-export([start_process/0,
        start_process/1,
        accept/1,
        start_server/0]).


-include("utils.hrl").
-include("config.hrl").

%% WORKER_NUMS    - how many process will spawn when server start
%% WORKER_TIMEOUT - an available process will exit after this timeout ,
%%                  this is used for reduce the spawned work process.

-define(CONNECT_RETRY_TIMES, 2).
-define(WORKER_NUMS, 30).
-define(WORKER_TIMEOUT, 600000).



-ifdef(DEBUG).
-define(LOG(Msg, Args), io:format(Msg, Args)).
-else.
-define(LOG(Msg, Args), true).
-endif.


start() ->
    {ok, Socket} = gen_tcp:listen(?REMOTEPORT, ?OPTIONS({0,0,0,0})),
    ?LOG("Server listen on ~p~n", [?REMOTEPORT]),
    register(proxy_gate, self()),
    register(server, spawn(?MODULE, start_server, [])),
    accept(Socket).


accept(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    server ! choosepid,
    receive
        {ok, Pid} ->
            ok = gen_tcp:controlling_process(Client, Pid),
            Pid ! {connect, Client},
            accept(Socket);
        {stop, From, Reason} ->
            From ! {ack, stop},
            ?LOG("Calling stop reason: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    after ?TIMEOUT ->
            gen_tcp:close(Client),
            accept(Socket)
    end.




start_server() ->
    loop(start_workers(?WORKER_NUMS)).

%% main loop, accept new connections, reuse workers, and purge dead workers.
loop(Workers) ->
    NewWorkers =
    receive
        choosepid ->
            manage_workers(choosepid, Workers);
        {'DOWN', _Ref, process, Pid, timeout} ->
            manage_workers(timeout, Workers, Pid);
        {reuse, Pid} ->
            manage_workers(reuse, Workers, Pid)
    end,
    loop(NewWorkers).


%% spawn some works as works pool.
start_workers(Num) ->
    start_workers(Num, []).

start_workers(0, Workers) ->
    Workers;
start_workers(Num, Workers) ->
    {Pid, _Ref} = spawn_monitor(?MODULE, start_process, []),
    start_workers(Num-1, [Pid | Workers]).



manage_workers(choosepid, []) ->
    [Head | Tail] = start_workers(?WORKER_NUMS),
    proxy_gate ! {ok, Head},
    Tail;

manage_workers(choosepid, [Head | Tail]) ->
    proxy_gate ! {ok, Head},
    Tail.

manage_workers(timeout, Works, Pid) ->
    ?LOG("Clear timeout pid: ~p~n", [Pid]),
    lists:delete(Pid, Works);

manage_workers(reuse, Works, Pid) ->
    ?LOG("Reuse Pid, back to pool: ~p~n", [Pid]),
    %% this reused pid MUST put at the tail or works list,
    %% for other works can be chosen and use.
    Works ++ [Pid].


start_process() ->
    receive
        {connect, Client} ->
            start_process(Client),
            server ! {reuse, self()},
            start_process()
    after ?WORKER_TIMEOUT ->
        exit(timeout)
    end.


start_process(Client) ->
    case gen_tcp:recv(Client, 1) of
        {ok, Data} ->
            parse_address(Client, proxy_transform:transform(Data));
        {error, _Error} ->
            ?LOG("start recv client error: ~p~n", [_Error]),
            gen_tcp:close(Client)
    end,
    ok.


parse_address(Client, AType) when AType =:= <<?IPV4>> ->
    {ok, Data} = gen_tcp:recv(Client, 6),
    <<Port:16, Destination/binary>> = proxy_transform:transform(Data),
    Address = list_to_tuple( binary_to_list(Destination) ),
    communicate(Client, Address, Port);

parse_address(Client, AType) when AType =:= <<?IPV6>> ->
    {ok, Data} = gen_tcp:recv(Client, 18),
    <<Port:16, Destination/binary>> = proxy_transform:transform(Data),
    Address = list_to_tuple( binary_to_list(Destination) ),
    communicate(Client, Address, Port);

parse_address(Client, AType) when AType =:= <<?DOMAIN>> ->
    {ok, Data} = gen_tcp:recv(Client, 3),
    <<Port:16, DomainLen:8>> = proxy_transform:transform(Data),

    {ok, DataRest} = gen_tcp:recv(Client, DomainLen),
    Destination = proxy_transform:transform(DataRest),

    Address = binary_to_list(Destination),
    communicate(Client, Address, Port);

parse_address(Client, _AType) ->
    %% receive the invalid data. close the connection
    ?LOG("Invalid data!~n", []),
    gen_tcp:close(Client).


communicate(Client, Address, Port) ->
    ?LOG("Address: ~p, Port: ~p~n", [Address, Port]),

    case connect_target(Address, Port, ?CONNECT_RETRY_TIMES) of
        {ok, TargetSocket} ->
            transfer(Client, TargetSocket);
        error ->
            ?LOG("Connect Address Error: ~p:~p~n", [Address, Port]),
            gen_tcp:close(Client)
    end.



connect_target(_, _, 0) ->
    error;
connect_target(Address, Port, Times) ->
    case gen_tcp:connect(Address, Port, ?OPTIONS, ?TIMEOUT) of
        {ok, TargetSocket} ->
            {ok, TargetSocket};
        {error, _Error} ->
            connect_target(Address, Port, Times-1)
    end.


transfer(Client, Remote) ->
    inet:setopts(Remote, [{active, once}]),
    inet:setopts(Client, [{active, once}]),
    receive
        {tcp, Client, Request} ->
            case gen_tcp:send(Remote, proxy_transform:transform(Request)) of
                ok ->
                    transfer(Client, Remote);
                {error, _Error} ->
                    ok
            end;
        {tcp, Remote, Response} ->
            %% client maybe close the connection when data transferring
            case gen_tcp:send(Client, proxy_transform:transform(Response)) of
                ok ->
                    transfer(Client, Remote);
                {error, _Error} ->
                    ok
            end;
        {tcp_closed, Client} ->
            ok;
        {tcp_closed, Remote} ->
            ok;
        {tcp_error, Client, _Reason} ->
            ok;
        {tcp_error, Remote, _Reason} ->
            ok
    end,

    gen_tcp:close(Remote),
    gen_tcp:close(Client),
    ok.
