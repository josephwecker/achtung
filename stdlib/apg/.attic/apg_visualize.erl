-module(apg_visualize).
-compile(export_all).


file_to_dot(InFName,OutFName) -> to_dot(apg_to_parser:file(InFName),OutFName).
string_to_dot(Txt,OutFName) -> to_dot(apg_to_parser:parse(Txt),OutFName).

to_dot(APGR, OutFName) ->
  DotTxt =
    f("digraph APG {") ++
    [V || {_K,V} <- dict:to_list(APGR:map(fun(R,E)-> rule_to_dot(R,E,APGR) end))] ++
    f("}"),
  file:write_file(OutFName, DotTxt).


rule_to_dot(RuleName, Expr, APGR) ->
  Name = str(RuleName),
  CName = f("cluster_~s",Name),
  [
    f("subgraph \"~s\" {",[CName]),
    f("compound=true;"),
    %f("node [style=filled,color=white];"),
    f("label = \"~s\";",Name),
    expr_to_dot(Expr,CName,APGR),
    f("}")
  ].

expr_to_dot([],_,_) -> [];
expr_to_dot({Type,Attr,L},_Entry,APGR) when is_list(L), Type =/= lit, Type =/= char ->
  [
    f("subgraph \"cluster_~s\" {", id(Attr)),
    f("label = \"~s\";",str(Type)),
    lists:map(fun(InnerExpr) -> expr_to_dot(InnerExpr,Type,APGR) end, L),
    f("}")
  ];
expr_to_dot({call,Attr,Rule}=E,_,APGR) ->
  %ID = f("~s~s",[str(Type),id(Attr)]),
  ID = node_id(E),
  Connect = node_id(first_node(APGR:fetch(Rule))),
  [
    f("~s [label=\"~s\"];",[ID,str(Rule)]),
    f("~s -> ~s [lhead:cluster_~s];", [ID, Connect, str(Rule)])

  ].
expr_to_dot({Type,Attr,Val},_,_APGR) ->
  [
    f("~s~s [label=\"~s\"];",[str(Type),id(Attr),str(Type)])
  ].


%first_node({

node_id({T,A,_}) -> f("~s~s",[str(T), id(A)]).

id(Attr) ->
  {L,C} = proplists:get_value(i,Attr,{0,0}),
  [f("_~b_~b",[L,C])].


%------------ String processing ---------------------------------------------|

f(Txt) -> " " ++ io_lib:format(Txt,[]) ++ " ".
f(Txt,Dat) when not is_list(Dat) -> f(Txt,[Dat]);
f(Txt,Dat) ->
  " " ++ io_lib:format(Txt,
    [case D of D1 when is_list(D1)->lists:flatten(D1);O->O end||D<-Dat]) ++
  " ".

str(A) when is_atom(A) -> escape_str(atom_to_list(A));
str(L) when is_list(L) -> escape_str(L);
str(I) when is_integer(I) -> escape_str(integer_to_list(I)).

chr_replace(Needle,Replacement,Haystack) ->
  chr_replace(Needle,Replacement,Haystack,[]).
chr_replace(_,_,[],A)   ->lists:flatten(lists:reverse(A));
chr_replace(C,R,[C|T],A)->chr_replace(C,R,T,[R|A]);
chr_replace(C,R,[H|T],A)->chr_replace(C,R,T,[H|A]).

escape_str(S) -> [chr_replace("\"","\\\"",chr_replace("\\", "\\\\", S))].


