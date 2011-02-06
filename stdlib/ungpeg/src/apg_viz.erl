-module(apg_viz).
-export([file_to_cec/2,string_to_cec/2,apgr_to_cec/2]).


% Canonical Expression Callgraph (CEC)
% Rule Callgraph (RC)
% Suffix Expression Callgraph (SEC)

file_to_cec(InFName,OutFName) -> apgr_to_cec(apg_to_parser:file(InFName),OutFName).
string_to_cec(Txt,OutFName) -> apgr_to_cec(apg_to_parser:parse(Txt),OutFName).

apgr_to_cec(APGR, OutFName) ->
  DotTxt =
    [
      fl("digraph APG {"),
      [V || {_K,V} <- dict:to_list(APGR:map(fun(R,E)-> rule_to_cec(R,E,APGR) end))],
      fl("}")
    ],
  file:write_file(OutFName, DotTxt).

rule_to_cec(Name, Body, _APGR) ->
  {EPNode, EPID} = entry_point(Name),
  {FailNode, FailID} = fail_point(Name),
  {SuccNode, SuccID} = succ_point(Name),
  FC = hd(lists:reverse(id(Body))) rem 8 + 2, % Assign somewhat random fail edge color
  SC = hd(lists:reverse(id(Body))) rem 8 + 2, % And for success
  cluster(Name,
    [
      EPNode,
      callgraph(Body, {Name,EPID}, [FailID], [SuccID], {FC,SC}),
      succ_edge(EPID, id(Body), SC),
      FailNode,
      SuccNode
    ]).

entry_point(Name) ->
  ID = f("ep_~s",Name),
  {fw("~s [label=\"'~s'\",style=\"filled\",shape=\"box\",fillcolor=\"#f8f8ee\"];",[ID,Name]), ID}.

fail_point(Name) ->
  ID = f("fp_~s",Name),
  {fw("~s [label=\"ϝ(~s)\",style=\"filled\",shape=\"box\",fillcolor=\"#ffeeee\"];",[ID,Name]), ID}.

succ_point(Name) ->
  ID = f("sp_~s",Name),
  {fw("~s [label=\"S(~s)\",style=\"filled\",shape=\"box\",fillcolor=\"#eeffee\"];",[ID,Name]), ID}.

cluster(Name,Body) ->
  ID = f("cluster_~s",Name),
  [
    fl("subgraph ~s {",ID),
    Body,
    fl("}\n")
  ].

callgraph({T,Attr,Val},_RecursePoint,[FID|_],[SID|_],{FC,SC}) when T==chr;T==lit ->
  ID = id(Attr),
  [node(T,ID,Val),fail_edge(ID,FID,FC),succ_edge(ID,SID,SC)];

callgraph({pch,_,EL},RecursePoint,[FID|_],[SID|_],Colors) ->
  Fails = tl([id(IE)||IE<-EL]) ++ [FID],
  lists:map(fun({InnerExp,InnerFailID})->
        callgraph(InnerExp,RecursePoint,[InnerFailID],[SID],Colors)
    end, lists:zip(EL,Fails));

% TODO: xch -> succ to all nodes to designate parallel?
%   Fail action in inner sequences shortcircuits to the xch's fail-id

callgraph({T,_,EL},RecursePoint,[FID|_],[SID|_],Colors) when T==seq ->
  Succs = tl([id(IE)||IE<-EL]) ++ [SID],
  lists:map(fun({InnerExp,InnerSuccID})->
        callgraph(InnerExp,RecursePoint,[FID],[InnerSuccID],Colors)
    end, lists:zip(EL,Succs));

callgraph(_,_,_,_,_) -> "".


node(chr,ID,Val) -> fl("~s [style=\"filled\",fillcolor=\"/greys9/2\",label=\"~s\"];",[ID,pp_ranges(Val)]);
node(lit,ID,Val) -> fl("~s [style=\"filled\",fillcolor=\"/greys9/3\",label=\"'~s'\"];",[ID,str(Val)]).
fail_edge(FromID, ToID, Color) -> fl("~s -> ~s [style=\"dashed\",color=\"/reds9/~s\"];",[FromID,ToID,Color]).
succ_edge(FromID, ToID, Color) -> fl("~s -> ~s [style=\"solid\",color=\"/greens9/~s\"];",[FromID,ToID,Color]).

id({T,_,[H|_]}) when T==seq;T==pch;T==xch -> id(H);
id({_T,Attr,_B}) -> id(Attr);
id(Attr) -> proplists:get_value(uid,Attr).


%------------ String processing ---------------------------------------------|


fl(Txt) -> [f(Txt),"\n"].
fl(Txt,Dat) -> [f(Txt,Dat),"\n"].
%fw(Txt) -> [f(Txt)," "].
fw(Txt,Dat) -> [f(Txt,Dat), " "].
f(Txt) -> io_lib:format(Txt,[]).
f(Txt,Dat) when not is_list(Dat) -> f(Txt,[Dat]);
f(Txt,Dat) ->
  EstRefCount = string:words(Txt,$~) - 1,
  Dat2 = case length(Dat) < EstRefCount of true -> lists:flatten(Dat); _->Dat end,
  iolist_to_binary(io_lib:format(Txt,
      [case D of D1 when is_list(D1)->lists:flatten(D1);O->str(O)
        end||D<-Dat2])).

str(A) when is_atom(A) -> escape_str(atom_to_list(A));
str(L) when is_list(L) -> escape_str(L);
str(I) when is_integer(I) -> integer_to_list(I);
str(B) when is_binary(B) -> escape_str(binary_to_list(B));
str(O) -> escape_str(lists:flatten(io_lib:format("~p",[O]))).
%tstr(O) -> escape_str(lists:flatten(io_lib:format("~p",[O]))).

chr_replace(Needle,Replacement,Haystack) -> chr_replace(Needle,Replacement,Haystack,[]).
chr_replace(_,_,[],A)   ->lists:flatten(lists:reverse(A));
chr_replace(C,R,[C|T],A)->chr_replace(C,R,T,[R|A]);
chr_replace(C,R,[H|T],A)->chr_replace(C,R,T,[H|A]).

escape_str(S) -> [chr_replace($\","\\\"",chr_replace($\\, "\\\\", S))].

pp_ranges(Ranges) ->
  lists:flatten(["[",lists:map(fun({C1,C2})->esc_str([C1,$-,C2]);(Ch)->esc_str(Ch) end,Ranges),"]"]).

esc_str(C) when is_integer(C) -> esc_str([C],[]);
esc_str(S) when is_list(S) -> esc_str(S,[]).
esc_str([],Acc) -> lists:reverse(Acc);
esc_str([$\t|R],Acc) -> esc_str(R,["\\t"|Acc]);
esc_str([$\n|R],Acc) -> esc_str(R,["\\n"|Acc]);
esc_str([$\r|R],Acc) -> esc_str(R,["\\r"|Acc]);
esc_str([N|R],Acc) when (N < $ ) or (N > 254) ->
  esc_str(R,[["\\x{",integer_to_list(N,16),"}"]|Acc]);
esc_str([C|R],Acc) -> esc_str(R,[C|Acc]).
