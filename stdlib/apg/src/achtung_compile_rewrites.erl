-module(achtung_compile_rewrites).
-export([file/1,parse/1]).

file(FName) -> crewrites(achtung_rewrite_n:file(FName)).
parse(Txt) when is_list(Txt) -> crewrites(achtung_rewrite_n:parse(Txt)).

crewrites({ok, _, AST}) ->
  AST2 = apply_aliases(AST),
  AST2.


apply_aliases(AST) ->
  {AliasTable,AST2} = sep_aliases(AST),
  ast_mapfold(fun(Node,Acc)-> expand_alias(Node,AliasTable,Acc) end, dict:new(), AST2).

sep_aliases(AST) -> sep_aliases(AST,dict:new(),[]).
sep_aliases([],AccA,AccR) -> {AccA,lists:reverse(AccR)};
sep_aliases([{alias,_,Name,Mapping}|R],AccA,AccR) -> sep_aliases(R,AccA:store(Name,Mapping),AccR);
sep_aliases([TLN|R],AccA,AccR) -> sep_aliases(R,AccA,[TLN|AccR]).

expand_alias({var,Pos,Name},Aliases,Seen) ->
  case Seen:find({Pos,Name}) of
    {ok,true} -> throw({"Recursive alias expansion.",Name,Pos,Seen:to_list()});
    error ->
      case Aliases:find(Name) of
        {ok, Expansion} -> {Expansion, Seen:store({Pos,Name},true)};
        error -> {{var,Pos,Name},Seen}
      end
  end;
expand_alias(N,_,Seen) -> {N,Seen}.


% Allows one to fold deeply within, whether the children terms are lists or
% tuples.
ast_mapfold(Fun,UserAcc,Node) when is_list(Node) ->
  {Node2,UserAcc2} = Fun(Node,UserAcc),
  lists:mapfoldl(fun(N,A)->ast_mapfold(Fun,A,N) end, UserAcc2, Node2);
ast_mapfold(Fun,UserAcc,Node) when is_tuple(Node) ->
  {Node2,UserAcc2} = Fun(Node,UserAcc),
  {Node3,UserAcc3} = lists:mapfoldl(fun(N,A)->ast_mapfold(Fun,A,N) end,
    UserAcc2, tuple_to_list(Node2)),
  {list_to_tuple(Node3),UserAcc3};
ast_mapfold(Fun,UserAcc,Node) ->
  Fun(Node,UserAcc).
  
