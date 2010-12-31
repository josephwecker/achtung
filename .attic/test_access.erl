% proplist vs. dict

-module(test_access).

-compile(export_all).


run_test(Num) ->
  {PL, DI} = prepare_data(Num),
  eprof:profile(fun()->
        run_proplists(PL,Num),
        run_dict(DI,Num),
        run_proplists(PL,Num),
        run_dict(DI,Num)
    end),
  %eprof:profile(fun()->run_proplists(PL,Num) end),
  %eprof:log("pl1.txt"),
  %eprof:analyze(),
  %eprof:profile(fun()->run_dict(DI,Num) end),
  %eprof:log("di1.txt"),
  %eprof:analyze(),
  %eprof:profile(fun()->run_proplists(PL,Num) end),
  %eprof:log("pl2.txt"),
  %eprof:analyze(),
  %eprof:profile(fun()->run_dict(DI,Num) end),
  %eprof:log("di2.txt"),
  eprof:analyze(),
  done.

prepare_data(Num) ->
  Dat = [{I,random:uniform(1000)} || I <- lists:seq(1,Num)],
  {Dat, dict:from_list(Dat)}.

run_proplists(Dat, Num) -> run_proplists(Dat, Num, none, 10000).
run_proplists(_, _, _, 0) -> done;
run_proplists(Dat, Num, _Last, Count) ->
  run_proplists(Dat, Num, proplists:get_value(random:uniform(Num - 1)+1, Dat), Count - 1).

run_dict(Dat, Num) -> run_dict(Dat, Num, none, 10000).
run_dict(_, _, _, 0) -> done;
run_dict(Dat, Num, _Last, Count) ->
  run_dict(Dat, Num, dict:fetch(random:uniform(Num - 1)+1, Dat), Count - 1).
