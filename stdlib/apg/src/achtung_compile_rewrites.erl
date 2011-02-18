-module(achtung_compile_rewrites).
-export([file/1,parse/2]).

file(FName) ->
  crewrites(achtung_rewrite_n:file(FName),module_from_filename(FName),FName).
parse(Txt,ModuleName) when is_list(Txt) ->
  crewrites(achtung_rewrite_n:parse(Txt),ModuleName,"").

crewrites({ok, _, AST},ModuleName,FName) ->
  AST2 = apply_aliases(AST),
  Forms = generate_forms(AST2,ModuleName,FName),
  % TODO: compile:forms(Forms)
  Forms.

%--------- Alias Expansion ---------------------------------------------------|
apply_aliases(AST) ->
  {AliasTable,AST2} = sep_aliases(AST),
  {AST3,_} = ast_mapfold(
    fun(Node,Acc)-> expand_alias(Node,AliasTable,Acc) end, dict:new(), AST2),
  AST3.
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

%--------- Generate Forms ----------------------------------------------------|
generate_forms(AST,Name,FName) ->
  TopMap = generate_topmap(AST),
  ExportNames = TopMap:fetch_keys() ++ TopMap:fetch(all),
  Exports = [{N,1}||N<-ExportNames],
  
  lists:flatten([{attribute,1,file,{FName,1}},
      {attribute,1,module,Name},
      {attribute,2,export,Exports},
      {eof,100}]).

generate_topmap(AST) -> generate_topmap(AST,dict:new()).
generate_topmap([],Map) -> Map;
generate_topmap([{mapping,_,NameParts,_}|R],Map) ->
  generate_topmap(
    R,append_tops(lists:reverse(NameParts),chain_atom(NameParts),Map));
generate_topmap([_|R],Map) -> generate_topmap(R,Map).

append_tops([],_,Map) -> Map;
append_tops([_|T],FullName,Map) ->
  append_tops(T,FullName,
    safe_append(Map,
      chain_atom(
        lists:reverse(case T of []->[all];_->T end)),FullName)).



%--------- Misc Utilities ----------------------------------------------------|
safe_append(Dict,Key,Value) ->
  case Dict:is_key(Key) of
    true -> Dict:append(Key,Value);
    false -> Dict:store(Key,[Value])
  end.

chain_atom(Chain)->list_to_atom(string:join([atom_to_list(C)||C<-Chain],"/")).

module_from_filename(FName) ->
  [Base|_] = string:tokens(filename:rootname(filename:basename(FName)),"."),
  list_to_atom(Base).

% Allows one to fold deeply within, whether the children terms are lists or tuples.
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
  
