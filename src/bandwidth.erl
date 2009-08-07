-module(bandwidth).
-export([client/2,server/0,bench/2]).
-import(bench_util,[repeat/2]).

client(Bytes,Repeat) ->
    receive
      ready -> ok
    end,
    Data = <<42:Bytes/unit:8>>,
    SomeHostInNet = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5678, 
                                 [binary, {packet, 0},
                                 {nodelay,true},
                                 {recbuf,1 bsl 18},
                                 {sndbuf,1 bsl 18}
                                 ]),
    repeat(Repeat,fun () ->
      ok = gen_tcp:send(Sock, Data)
    end),
    ok = gen_tcp:close(Sock).

server() ->
    receive
      {ready,Who} -> ok
    end,
    {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0}, 
                                        {active, false},
                                        {nodelay,true},
                                        {recbuf,1 bsl 18},
                                        {sndbuf,1 bsl 18}
                                        ]),
    {ok, Sock} = gen_tcp:accept(LSock),
    T1 = now(),
    {ok, Size} = do_recv(Sock, 0),
    ok = gen_tcp:close(Sock),
    T2 = now(),
    McS = timer:now_diff(T2,T1),
    Who ! {McS / 1000000.0,Size}.

do_recv(Sock, Bs) ->
    try
      inet:setopts(Sock,[{active,once}])
    catch
      error:closed -> {error,Bs}
    end,
    receive
        {tcp,Sock,B} ->
            do_recv(Sock, Bs+byte_size(B));
        {tcp_closed,Sock} ->
            {ok, Bs}
    end.

bench(Bytes,Repeat) ->
  io:format("test bandwidth: ~p bytes x ~p repetitions~n",[Bytes,Repeat]),
  Client = spawn(bandwidth,client,[Bytes,Repeat]),
  Server = spawn(bandwidth,server,[]),
  Server ! {ready,self()},
  timer:sleep(100), % give it time to start listening
  Client ! ready,
  receive
    {Seconds,Transmitted} -> ok
  end,
  Megabits = 8 * Transmitted / Seconds / 1000000,
  io:format("result bandwidth: ~p bytes x ~p seconds = ~p megabits~n",[Transmitted,Seconds,Megabits]),
  true.

