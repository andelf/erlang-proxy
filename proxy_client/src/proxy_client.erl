-module(proxy_client).

-export([start/0]).

-export([accept_loop/1]).

-include("proxy_defs.hrl").



-define(SOCK_OPTIONS,
        [binary,
         {reuseaddr, true},
         {active, false},
         {nodelay, true}
        ]).


start() ->
    ConfFile = filename:join(code:priv_dir(proxy_client), "client.conf"),
    case file:consult(ConfFile) of
        {ok, Conf} ->
            ListenPort = proplists:get_value(listen_port, Conf),
            ListenIP = proplists:get_value(listen_ip, Conf);
        {error, _} ->
            ListenPort = 7070,
            ListenIP = {127,0,0,1}
    end,
    {ok, Socket} = gen_tcp:listen(ListenPort, [{ip, ListenIP} | ?SOCK_OPTIONS]),
    ?LOG("Porxy client listen on ~p : ~p~n", [ListenIP, ListenPort]),
    accept_loop(Socket).


accept_loop(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    {ok, Pid} = supervisor:start_child(proxy_client_worker_sup, [Client]),
    ok = gen_tcp:controlling_process(Client, Pid),
    accept_loop(Socket).
