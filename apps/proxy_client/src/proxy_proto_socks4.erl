%%%-------------------------------------------------------------------
%%% @author Wang ShuYu <andelf@gmail.com>
%%% @copyright (C) 2013, Wang ShuYu
%%% @doc
%%%
%%% @end
%%% Created : 10 Apr 2013 by Wang ShuYu <andelf@gmail.com>
%%%-------------------------------------------------------------------
-module(proxy_proto_socks4).

%% API
-export([]).
-compile([export_all]).

%%%===================================================================
%%% API
%%%===================================================================
parse_greeting_request(<<16#04:8, CmdCode:8, Port:16, Address:32, Rest/binary>>) ->
    case CmdCode of
        16#01 when Address =< 16#FF ->
            [UserId, DomainBin, <<>>] = binary:split(Rest, <<0>>, [global]),
            {connect, UserId, {domain, DomainBin, Port}};
        16#01 ->
            [UserId, <<>>] = binary:split(Rest, <<0>>, [global]),
            {connect, UserId, {ipv4, Address, Port}};
        16#02 ->
            {error, not_implemented_yet}
    end.

unparse_greeting_response(granted) ->
    <<0:8, 16#5a:8, 16#FFFF:16, 16#FFFFFFFF:32>>;
unparse_greeting_response(rejected) ->
    <<0:8, 16#5b:8, 16#FFFF:16, 16#FFFFFFFF:32>>.

unparse_connection_response({granted, _}) ->
    unparse_greeting_response(granted);
unparse_connection_response({rejected, _}) ->
    unparse_greeting_response(rejected).

%%%===================================================================
%%% Internal functions
%%%===================================================================
