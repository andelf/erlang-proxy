-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).

-define(OPTIONS,
		[binary,
		{reuseaddr, true},
		{active, false},
		{nodelay, true}
        ]).

-define(OPTIONS(IP), [{ip, IP} | ?OPTIONS]).

-define(GETADDR, fun(IP) -> {ok, Addr} = inet:getaddr(IP, inet), Addr end).

-ifdef(DEBUG).
-define(LOG(Msg, Args), io:format(Msg, Args)).
-define(LOG(Msg), io:format(Msg)).
-else.
-define(LOG(Msg, Args), true).
-define(LOG(Msg), true).
-endif.
