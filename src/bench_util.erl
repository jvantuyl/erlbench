-module(bench_util).
-export([repeat/2]).

repeat(0,_) -> done;
repeat(N,F) -> F(), repeat(N-1,F).
