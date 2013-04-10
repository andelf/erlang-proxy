%%%-------------------------------------------------------------------
%%% @author Wang ShuYu <andelf@gmail.com>
%%% @copyright (C) 2013, Wang ShuYu
%%% @doc
%%%
%%% @end
%%% Created : 10 Apr 2013 by Wang ShuYu <andelf@gmail.com>
%%%-------------------------------------------------------------------
-module(proxy_proto_socks5).

%% API
-export([]).
-compile([export_all]).

-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).


%%%===================================================================
%%% API
%%%===================================================================
parse_greeting_request(<<16#05:8, NumMethods:8, Methods:NumMethods/binary>>) ->
    {auth_methods, Methods}.

unparse_greeting_response(Method) ->
    MethodProp = [{no_auth, 16#00},
                  {user_pass, 16#02}],
    Code = proplists:get_value(Method, MethodProp),
    <<16#05:8, Code:8>>.

parse_auth_request(user_pass, _) ->
    {error, not_implemented_yet}.

parse_connection_request(<<16#05:8, CmdCode:8, 0:8, AddrType:8, Rest/binary>>) ->
    case CmdCode of
        %% CONNET
        16#01 ->
            case AddrType of
                ?IPV4 ->
                    <<Address:32, Port:16>> = Rest,
                    {connect, {ipv4, Address, Port}};
                ?IPV6 ->
                    <<Address:128, Port:16>> = Rest,
                    {connect, {ipv6, Address, Port}};
                ?DOMAIN ->
                    <<DomainLen:8, DomainBin:DomainLen/binary, Port:16>> = Rest,
                    %<<?DOMAIN, Port:16, DomainLen:8, DomainBin/binary>>
                    {connect, {domain, DomainBin, Port}}
            end;
        %% BIND
        16#02 ->
            {error, not_implemented_yet};
        %% UDP ASSOCIATE
        16#03 ->
            {error, not_implemented_yet}
    end.

unparse_connection_response({granted, {ipv4, IP, Port}}) ->
    <<16#05:8, 16#00:8, 0:8, ?IPV4, IP/binary, Port:16>>;
unparse_connection_response({granted, {ipv6, IP, Port}}) ->
    <<16#05:8, 16#00:8, 0:8, ?IPV6, IP/binary, Port:16>>;
unparse_connection_response({granted, {domain, DomainBin, Port}}) ->
    <<16#05:8, 16#00:8, 0:8, ?DOMAIN, (byte_size(DomainBin)):8, DomainBin/binary, Port:16>>;
unparse_connection_response({rejected, _}) ->
    <<16#05:8, 16#03:8, 0:8>>.



%%%===================================================================
%%% Internal functions
%%%===================================================================
