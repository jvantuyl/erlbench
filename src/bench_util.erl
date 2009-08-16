-module(bench_util).
-export([repeat/2]).

repeat(0,_) -> done;
repeat(N,F) when is_integer(N) -> F(), repeat(N-1,F).
