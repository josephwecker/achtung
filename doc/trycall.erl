%
%
% TODO:
%  - Parser (or do_call) distinguishing between package chain and properties
%  returning types with more properties:
%      utils.net.socket_server.start()
%  vs. my_record.other_record.has_default_fun()
%  vs. utils/net/socket_server.start     <------   LGTM
%
%  - Specialized tuple handler on exception when do_call(PM...) fails...
%  - Better yet, some tuple-handling functions/properties that are always
%    there and can't be overridden... (?)
%  - Define clearly when do_call is not necessary (for the parser)
%  - Try and move recursive parts of do_call into its own function and trigger
%    only when absolutely necessary (even though it won't look as clean)
%
% -----
%  - Need to make sure that it's really essentially a macro! so inline it
%    ourselves so that dialyzer isn't thrown off.
%

-module(trycall).

-compile([export_all, {inline, [do_call/2]}]).

-record(tester, {f_one=3, f_two=[blah]}).

test() ->
  L = [1,2,3],                                              %  L  = [1,2,3]
  L2= do_call(L, reverse),                                  %  L2 = L.reverse()
  D = dict:new(),                                           %  D  = dict.new()
  D2= do_call(D, append, [a, <<"value!">>]),                %  D2 = D.append(a <<"value!">>)
  A = do_call(D2, fetch_keys),                              %  A  = D2.fetch_keys()

  R = #tester{f_one=4, f_two=[weirdo]},                             % R  = tester[f_one:=4 f_two:=[weirdo]]
  R2= do_call(R, ung_array_access, [{f_one,8},{f_two,[boo, hoo]}]), % R2 = R[f_one:=8 f_two:=[boo hoo]]
  RV= do_call(R, ung_property, [f_one]),                            % RV = R.f_one
  RV= do_call(R, ung_array_access, [f_one]),                        % RV = R[f_one]
  RaV=do_call(R, ung_array_access, [f_one, f_two]),                 % RaV= R[f_one f_two]
  RV2=
    do_call(
      do_call(
        do_call(R, ung_array_access, [{f_one, [silly]}]),
        ung_array_access, [{f_two,9}]),
      ung_property, [f_one]),                                       % RV2= R2[f_one:=[silly]][f_two:=9].f_one

  M = math,
  V = 2,
  SQ= do_call(M, sqrt, [V]),
  
  io:format("~p", [{L, L2, D, D2, A, R, R2, RV, RaV, RV2, SQ}]).

do_call(V, FunName) -> do_call(V, FunName, []).
% "Standard" data structure parameterized module
% Specialized list- no other parameters
do_call(V, FunName, []) when is_list(V) -> lists:FunName(V);
do_call(V, FunName, Params) when is_list(V) -> apply(lists,FunName,Params++[V]);
% Custom built for record "tester"
% Assignments
do_call({tester,_,_}=Rec, ung_array_access, []) -> Rec;
do_call({tester,_,F2}, ung_array_access, [{f_one,V}|R]) -> do_call({tester,V,F2}, ung_array_access, R);
do_call({tester,F1,_}, ung_array_access, [{f_two,V}|R]) -> do_call({tester,F1,V}, ung_array_access, R);
% Access
do_call({tester,_,_}=Rec, ung_array_access, [I]) -> do_call(Rec, ung_property, [I]);
do_call({tester,_,_}=Rec, ung_array_access, L) -> [do_call(Rec, ung_property, [LI])||LI<-L];
do_call({tester,F1,_}, ung_property, [f_one]) -> F1;
do_call({tester,_,F2}, ung_property, [f_two]) -> F2;
% General parameterized module call (or normal module call)
do_call(V, FunName, Params) -> apply(V, FunName, Params).

