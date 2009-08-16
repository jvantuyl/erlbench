-module(bandwidth).
-export([client/2,server/0,bench/2]).
-import(bench_util,[repeat/2]).

client(Bytes,Repeat) ->
    receive % Wait for start signal
      ready -> ok
    end,
    Data = <<42:Bytes/unit:8>>, % Prepare Data Block
    SomeHostInNet = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5678,  % connect
                                 [binary, {packet, 0},
                                 {nodelay,true},
                                 {recbuf,1 bsl 18},
                                 {sndbuf,1 bsl 18}
                                 ]),
    DoSend = fun () -> % Function to send a block
      ok = gen_tcp:send(Sock, Data)
    end,
    repeat(Repeat,DoSend), % Repeat function
    ok = gen_tcp:close(Sock), % close socket
    done.

server() ->
    receive % Wait for start signal, snag initiator PID
      {ready,Who} -> ok
    end,
    {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0}, % Listen for
                                        {active, false},     % connection
                                        {nodelay,true},
                                        {recbuf,1 bsl 18},
                                        {sndbuf,1 bsl 18}
                                        ]),
    {ok, Sock} = gen_tcp:accept(LSock), % Accept connection
    T1 = now(), % Time start
    {ok, Size} = do_recv(Sock, 0), % receive until it's gone
    ok = gen_tcp:close(Sock), % Clean Up
    T2 = now(), % Time stop
    ok = gen_tcp:close(LSock), % Clean Up More
    McS = timer:now_diff(T2,T1), % Calculate Time Difference
    Who ! {results,McS / 1000000.0,Size}, % send Results
    done.

do_recv(Sock, Bs) ->
    try
      inet:setopts(Sock,[{active,once}]) % Set to active-once
    catch
      error:closed -> self() ! {tcp_closed,Sock} % Synthesize close message
    end,
    receive % Receive Data or close
        {tcp,Sock,B} ->
            do_recv(Sock, Bs+byte_size(B)); % More data, recurse
        {tcp_closed,Sock} ->
            {ok, Bs} % Done, return
    end.

bench(Bytes,Repeat) ->
  % header
  io:format("test bandwidth: ~p bytes x ~p repetitions~n",[Bytes,Repeat]),
  Client = spawn_link(bandwidth,client,[Bytes,Repeat]), % client
  Server = spawn_link(bandwidth,server,[]), % server
  % Note that the links cause us to crash if the other processes crash
  Server ! {ready,self()}, % Poke server
  timer:sleep(100), % give it time to start listening
  Client ! ready, % Poke client
  receive % wait for results
    {results,Seconds,Transmitted} -> ok
  end,
  Megabits = 8 * Transmitted / Seconds / 1000000, % Massage results
  io:format("result bandwidth: ~p bytes x ~p seconds = ~p megabits~n",
    [Transmitted,Seconds,Megabits]), % Display results
  true.
