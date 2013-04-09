%%%-------------------------------------------------------------------
%%% @author Wang ShuYu <andelf@gmail.com>
%%% @copyright (C) 2013, Wang ShuYu
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2013 by Wang ShuYu <andelf@gmail.com>
%%%-------------------------------------------------------------------
-module(proxy_client_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {server_ip,
                server_port,
                server_sock,
                client_ip,
                client_port,
                client_sock
               }).

-define(SOCK_OPTIONS,
        [binary,
         {reuseaddr, true},
         {active, false},
         {nodelay, true}
        ]).

-include("proxy_defs.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
% Leave name {local, name} so that the process
% remains unregistered. This enables us to start
% mutliple processes using the pr_sup:start_child() call
start_link(ClientSock) ->
    ConfFile = filename:join(code:priv_dir(proxy_client), "client.conf"),
    case file:consult(ConfFile) of
        {ok, Conf} ->
            gen_server:start_link(?MODULE, [{client_sock, ClientSock}|Conf], []);
        {error, _Reason} ->
            {error, conf_file_error}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Conf) ->
    ServerIP = proplists:get_value(server_ip, Conf),
    ServerPort = proplists:get_value(server_port, Conf),
    ClientIP = proplists:get_value(listen_ip, Conf),
    ClientPort = proplists:get_value(listen_port, Conf),
    Client = proplists:get_value(client_sock, Conf),
    case gen_tcp:connect(getaddr_or_fail(ServerIP), ServerPort, ?SOCK_OPTIONS) of
        {ok, RemoteSocket} ->
            ok = inet:setopts(RemoteSocket, [{active, true}]),
            {ok, #state{server_ip=ServerIP,
                        server_port=ServerPort,
                        server_sock=RemoteSocket,
                        client_ip=ClientIP,
                        client_port=ClientPort,
                        client_sock=Client}, 0};
                 %%communicate(Client, RemoteSocket);
        {error, Error} ->
            ?LOG("Connect error, ~p. ~p:~p~n", [Error, ServerIP, ServerPort]),
            gen_tcp:close(Client),
            {stop, server_connect_fail}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{server_sock=RemoteSocket, client_sock=Client, client_ip=LocalIP, client_port=LocalPort} = State) ->
    try
        Target = find_target(Client),
        ok = inet:setopts(Client, [{active, true}]),
        try
            ok = gen_tcp:send(RemoteSocket, proxy_transform:transform(Target)),
            IP = list_to_binary(tuple_to_list(getaddr_or_fail(LocalIP))),
            ok = gen_tcp:send(Client, <<5, 0, 0, 1, IP/binary, LocalPort:16>>),
            {noreply, State}
        catch
            error:Reason ->
                ?LOG("client communication error: ~p~n", [Reason]),
                {stop, Reason, State}
        end
    catch
        error:{badmatch,_} ->
            {stop, normal, State};
        _Error:_Reason ->
            ?LOG("client recv error, ~p: ~p~n", [_Error, _Reason]),
            {stop, normal, State}
    end;
handle_info({tcp, Client, Request}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    case gen_tcp:send(RemoteSocket, proxy_transform:transform(Request)) of
        ok ->
            {noreply, State};
        {error, _Error} ->
            {stop, _Error, State}
    end;
handle_info({tcp, RemoteSocket, Response}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    case gen_tcp:send(Client, proxy_transform:transform(Response)) of
        ok ->
            {noreply, State};
        {error, _Error} ->
            {stop, _Error, State}
    end;
handle_info({tcp_closed, ASocket}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    case ASocket of
        Client ->
            {stop, normal, State};
        RemoteSocket ->
            {stop, normal, State}
    end;
handle_info({tcp_error, ASocket, _Reason}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    case ASocket of
        Client ->
            ?LOG("~p client tcp error~n", [ASocket]),
            {stop, _Reason, State};
        RemoteSocket ->
            ?LOG("~p server tcp error~n", [ASocket]),
            {stop, _Reason, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason,  #state{server_sock=RemoteSocket, client_sock=Client}) ->
    gen_tcp:close(RemoteSocket),
    gen_tcp:close(Client),
    ok;
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
getaddr_or_fail(IP) ->
    {ok, Addr} = inet:getaddr(IP, inet),
    Addr.


find_target(Client) ->
    {ok, <<16#05:8, Nmethods:8>>} = gen_tcp:recv(Client, 2),
    {ok, _Methods} = gen_tcp:recv(Client, Nmethods),

    gen_tcp:send(Client, <<16#05, 16#0>>),
    {ok, <<16#05:8, 16#01:8, _Rsv:8, AType:8>>} = gen_tcp:recv(Client, 4),

    case AType of
        ?IPV4 ->
            {ok, <<Address:32>>} = gen_tcp:recv(Client, 4),
            {ok, <<Port:16>>} = gen_tcp:recv(Client, 2),
            <<?IPV4, Port:16, Address:32>>;
        ?IPV6 ->
            {ok, <<Address:128>>} = gen_tcp:recv(Client, 16),
            {ok, <<Port:16>>} = gen_tcp:recv(Client, 2),
            <<?IPV6, Port:16, Address:128>>;
        ?DOMAIN ->
            {ok, <<DomainLen:8>>} = gen_tcp:recv(Client, 1),
            {ok, <<DomainBin/binary>>} = gen_tcp:recv(Client, DomainLen),
            {ok, <<Port:16>>} = gen_tcp:recv(Client, 2),
            <<?DOMAIN, Port:16, DomainLen:8, DomainBin/binary>>
    end.
