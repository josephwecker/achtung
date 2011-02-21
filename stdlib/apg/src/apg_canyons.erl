-module(apg_canyons).
-export([draw_result/2]).





draw_result(FName, Form) ->
  file:write_file(FName, [
    "digraph ast {",
      "edge [color=\"#666666\",arrowsize=0.4];",
      "node [fontname=\"Helvetica\",style=filled,",
      "fillcolor=\"#f8f8ff\",color=\"#666677\",fontcolor=\"#444455\"];",
      draw_nodes(Form),
    "}"]),
  os:cmd(binary_to_list(f("dot -Tsvg -o '~s' '~s'",[svg_name(FName),FName]))).

svg_name(DotName) -> f("~s.svg",filename:rootname(DotName,".dot")).

draw_nodes(Form) ->
  D = dict:new(),
  {Groups,Conns} = traverse_nodes(mark(Form),uid(g),D:store(1,[]),[],1),
  Conns2 = lists:reverse(Conns),
  [
    [["{rank=same;",[nrank(N)||N<-Nodes],"}\n"] || {_,Nodes} <- lists:sort(Groups:to_list())],
    [edge(From,To) ||{From,To} <- lists:zip(['']++Conns2,Conns2++['']),From=/='',To =/='']
  ].

traverse_nodes([[_|_]=L],_CurrGroup,Groups,Conns,Depth) ->
  traverse_nodes(L,f("g~s",[Depth]),Groups,Conns,Depth+1);
  %traverse_nodes(L,uid(g),Groups,Conns);
traverse_nodes([V],CurrGroup,Groups,Conns,_Depth) ->
  {Groups:append(CurrGroup,V), [V|Conns]};
traverse_nodes([[_|_]=L|R],CurrGroup,Groups,Conns,Depth) ->
  {Groups2,Conns2} = traverse_nodes(L,f("g~s",[Depth]),Groups,Conns,Depth+1),
  %{Groups2,Conns2} = traverse_nodes(L,uid(g),Groups,Conns),
  traverse_nodes(R,CurrGroup,Groups2,Conns2,Depth);
traverse_nodes([V|R],CurrGroup,Groups,Conns,Depth) ->
  traverse_nodes(R,CurrGroup,Groups:append(CurrGroup,V),[V|Conns],Depth).

nrank({_,[$h|_]=ID}) -> f("~s[shape=point];",ID);
nrank({_,[$t|_]=ID}) -> f("~s[shape=point];",ID);
nrank({Label,ID}) -> f("~s[label=\"~s\"];",[ID,Label]).

edge({_,FromID},{_,[$h|_]=ToID}) -> f("~s -> ~s [dir=none];\n",[FromID,ToID]);
edge({_,FromID},{_,[$t|_]=ToID}) -> f("~s -> ~s [dir=none];\n",[FromID,ToID]);
edge({_,FromID},{_,ToID}) -> f("~s -> ~s;\n",[FromID,ToID]).

mark(L) -> mark(L,[]).
mark([],A) -> lists:reverse(A);
mark([L|R],A) when is_list(L)->mark(R,[[{"",uid(h)}]++mark(L,[])++[{"",uid(t)}]|A]);
mark([V|R],A)->mark(R,[{str(V),uid(n)}|A]).

%--------------- Helper functions I really need to give a home to ------------|
uid(Type) -> atom_to_list(Type)++"_"++chr_replace($.,"_",lists:nth(2,string:tokens(erlang:ref_to_list(make_ref()),"<>"))).
f(Txt) -> io_lib:format(Txt,[]).
f(Txt,Dat) when not is_list(Dat) -> f(Txt,[Dat]);
f(Txt,Dat) ->
  EstRefCount = string:words(" "++Txt,$~) - 1,
  Dat2 =
    case {length(Dat),EstRefCount} of
      {L,1} when L > 1 -> [lists:flatten(Dat)];
      {L,C} when L == C -> [str(ID)||ID<-Dat];
      _ -> Dat
    end,
  iolist_to_binary(io_lib:format(Txt,Dat2)).
str(A) when is_atom(A) -> escape_str(atom_to_list(A));
str(L) when is_list(L) -> escape_str(L);
str(I) when is_integer(I) -> integer_to_list(I);
str(B) when is_binary(B) -> escape_str(binary_to_list(B));
str(O) -> escape_str(lists:flatten(io_lib:format("~p",[O]))).
escape_str(S) -> [chr_replace($\","\\\"",chr_replace($\\, "\\\\", S))].
chr_replace(Needle,Replacement,Haystack) -> chr_replace(Needle,Replacement,Haystack,[]).
chr_replace(_,_,[],A)   ->lists:flatten(lists:reverse(A));
chr_replace(C,R,[C|T],A)->chr_replace(C,R,T,[R|A]);
chr_replace(C,R,[H|T],A)->chr_replace(C,R,T,[H|A]).
