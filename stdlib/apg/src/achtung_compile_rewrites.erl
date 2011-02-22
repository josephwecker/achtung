-module(achtung_compile_rewrites).
-export([file/1,parse/2]).

-define(POS,{{line,Pos},_}).
-define(R(T),lists:reverse(T)).

file(FName) ->
  crewrites(achtung_rewrite_n:file(FName),module_from_filename(FName),FName).
parse(Txt,ModuleName) when is_list(Txt) ->
  crewrites(achtung_rewrite_n:parse(Txt),ModuleName,"").

crewrites({ok, _, AST},ModuleName,FName) ->
  AST2 = apply_aliases(AST),
  Forms = generate_forms(AST2,ModuleName,FName),
  %io:format("~p~n",[Forms]),
  io:format("~s~n",[erl_prettypr:format(erl_syntax:form_list(Forms))]),
  {ok, Module, Bin} = compile:forms(Forms),
  code:soft_purge(Module),
  code:load_binary(Module,FName,Bin).

%--------- Alias Expansion ---------------------------------------------------|
apply_aliases(AST) ->
  {AliasTable,AST2} = sep_aliases(AST),
  {AST3,_} = ast_mapfold(
    fun(Node,Acc)-> expand_alias(Node,AliasTable,Acc) end, dict:new(), AST2),
  AST3.
sep_aliases(AST) -> sep_aliases(AST,dict:new(),[]).
sep_aliases([],AccA,AccR) -> {AccA,?R(AccR)};
sep_aliases([{alias,_,Name,Mapping}|R],AccA,AccR) ->
  sep_aliases(R,AccA:store(Name,Mapping),AccR);
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
  io:format("~p~n~n",[AST]),
  %io:format("~p~n~n~p~n~n",[AST,TopMap:to_list()]),
  GFuns = generate_group_funs(TopMap),
  Functions = generate_functions(AST),
  
  lists:flatten([{attribute,1,file,{FName,1}},
      {attribute,1,module,atom(Name)},
      {attribute,2,export,Exports},
      GFuns,
      Functions,
      {eof,100}]).

generate_topmap(AST) -> generate_topmap(AST,dict:new()).
generate_topmap([],Map) -> Map;
generate_topmap([{mapping,_,NameParts,_}|R],Map) ->
  generate_topmap(
    R,append_tops(?R(NameParts),chain_atom(NameParts),Map));
generate_topmap([_|R],Map) -> generate_topmap(R,Map).

append_tops([],_,Map) -> Map;
append_tops([_|T],FullName,Map) ->
  append_tops(T,FullName,
    safe_append(Map,
      chain_atom(
        ?R(case T of []->[all];_->T end)),FullName)).

generate_group_funs(Map) ->
  lists:map(fun group_fun/1,Map:to_list()).
group_fun({Name,[OneFun]}) ->
  {function,1,Name,1,[{clause,1,[{var,1,'T'}],[],[
          {call,1,{atom,1,OneFun},[{var,1,'T'}]}
        ]}]};
group_fun({Name,Inners}) ->
  {function,1,Name,1, [{clause,1,[{var,1,'T'}], [],
        [{'case',1,
            lists:foldl(fun(F,Chain)->{call,1,{atom,1,F},[Chain]} end,
              {var,1,'T'}, Inners),
          [{clause,1,[{var,1,'T'}],[],[{var,1,'T'}]},
           {clause,1,[{var,1,'T2'}],[],
             [{call,1,{atom,1,Name},[{var,1,'T2'}]}]}]}]}]}.

generate_functions(AST) -> generate_functions(AST,[]).
generate_functions([],Acc) -> ?R(Acc);
generate_functions([{mapping,{{line,L},_},NameParts,Clauses}|R],Acc) ->
  F = {function,L,chain_atom(NameParts),1,generate_clauses(Clauses)},
  generate_functions(R,[F|Acc]);
generate_functions([_|R],Acc) -> generate_functions(R,Acc).

generate_clauses(RWCs) -> generate_clauses(RWCs,[]).
generate_clauses([],Acc) -> ?R(Acc);
generate_clauses([{rwclause,_Pos,Left,Qual,Right}|R], Acc) ->
  generate_clauses(R,[fclause(Left,Qual,Right)|Acc]).

fclause(Left,Qual,Right) ->
  Left2 = enumerate_anons(Left),
  {LeftPattern, LSigs} = normalize_left(Left2,[]),
  WhenClause = Qual ++ [{call,P,{atom,P,is_list},[{var,P,AVName}]}||
    {{var,P,AVName},_} <- LSigs, used(AVName,LeftPattern)],
  WhenClause2 = case WhenClause of [] -> []; WList -> [WList] end,
  io:format("LEFTPATTERN:~n~p~n~nLEFTSIGS:~n~p~n-----~n",[LeftPattern,LSigs]),
  Right2 = enumerate_anons(Right),
  {RightExpr, _LSigs2} = normalize_right(hd(Right2), []),  % TEMPORARY
  %{NextFun, RightExpr} = normalize_right(Right, LSigs),

  % TODO: chain of case statements so that variables are bound - check to see
  % if they're used in result and prefix with underscores as appropriate (don't
  % completely replace with underscore because that would break trying to match
  % the same binding more than once in the left-clause)

  {clause,1,[LeftPattern],WhenClause2,[RightExpr]}.


enumerate_anons(T) ->
  put(agg_count,1),
  {T2, _} = ast_mapfold(fun enum_anon/2, nil, T),
  T2.

enum_anon('__ARW_ANON_R'=N,_) -> {{var,{{line,0},nil},next_agg(N)},nil};
enum_anon('__ARW_ANON_L'=N,_) -> {{var,{{line,0},nil},next_agg(N)},nil};
enum_anon('__ARW_ANON_M'=N,_) -> {{var,{{line,0},nil},next_agg(N)},nil};
enum_anon(V,_) -> {V,nil}.

normalize_left({atom,?POS,Val},Acc) ->      {{atom,Pos,Val}, Acc};
normalize_left(A,Acc) when is_atom(A) ->    {{atom,0,A},Acc};
normalize_left({lsig,?POS,[]},Acc) ->       {{nil,Pos},Acc};
normalize_left([],Acc) ->                   {{nil,0},Acc};
normalize_left({match_term,_P,V},Acc) ->    normalize_left(V,Acc);
normalize_left({agg_term,_P,V},Acc) ->      normalize_left(V,Acc);
normalize_left({tuple,?POS,L},Acc) ->
  {Pattern, Acc2} = lists:mapfoldl(fun normalize_left/2, Acc, L),
  {{tuple,Pos,Pattern}, Acc2};
normalize_left({var,?POS,Val},Acc) ->
  Val2 = list_to_atom(atom_to_list(Val)++"_ARW"),
  {{var,Pos,Val2}, Acc};
normalize_left({lsig,?POS,L},Acc) ->
  {H,T} = lists:splitwith(fun({match_term,_,_})->true;(_)->false end, L),
  {L2,Acc2} = lists:mapfoldl(fun normalize_left/2, Acc, H),
  case T of
    [] -> {conses(L2,Pos), Acc2};
    [T1] ->
      {T2,Acc3} = normalize_left(T1,Acc2),
      {conses(L2,Pos,T2), Acc3};
    [T1|_TR]=TList ->
      {VarName, Acc3} = normalize_left(T1,Acc2),
      {conses(L2,Pos,VarName), [{VarName, TList}|Acc3]}
  end.

normalize_right({atom,?POS,Val},Acc) ->      {{atom,Pos,Val}, Acc};
normalize_right(A,Acc) when is_atom(A) ->    {{atom,0,A},Acc};
normalize_right({lsig,?POS,[]},Acc) ->       {{nil,Pos},Acc};
normalize_right([],Acc) ->                   {{nil,0},Acc};
normalize_right({match_term,_P,V},Acc) ->    normalize_right(V,Acc);
normalize_right({agg_term,_P,V},Acc) ->      normalize_right(V,Acc);
normalize_right({tuple,?POS,L},Acc) ->
  {Pattern, Acc2} = lists:mapfoldl(fun normalize_right/2, Acc, L),
  {{tuple,Pos,Pattern}, Acc2};
normalize_right({var,?POS,Val},Acc) ->
  Val2 = list_to_atom(atom_to_list(Val)++"_ARW"),
  {{var,Pos,Val2}, Acc}.


conses(L,Pos) -> conses(L,Pos,{nil,Pos}).
conses(L,Pos,[]) -> conses(L,Pos,{nil,Pos});
conses([],_,Tail) -> Tail;
conses([H|R],Pos,Tail) ->
  {cons,Pos,H,
    case R of
      []    -> Tail;
      [_|_] -> conses(R,Pos,Tail)
    end}.

used(Name,Name) -> true;
used(Name,T) when is_tuple(T) ->
  lists:any(fun(E)->used(Name,E) end, tuple_to_list(T));
used(Name,L) when is_list(L) ->
  lists:any(fun(E)->used(Name,E) end, L);
used(_,_) -> false.

next_agg(Pref) when is_atom(Pref) -> next_agg(atom_to_list(Pref));
next_agg(Pref) ->
  C = case get(agg_count) of undefined -> 0; V -> V end,
  put(agg_count, C + 1),
  list_to_atom(Pref ++ integer_to_list(C)).


  %  1. Left-side Toplevel clause pattern to erlang pattern-
  %     * All variables given new prefixes
  %     * Number all ignored/anon aggs so they automatically match up on the
  %       other side.
  %     * Listsigs turned into normal match-exprs if possible (implies no aggs
  %       except possibly one at the end.)
  %     * (Otherwise turn them into L-variables & is_list(L<n>) pairs.
  %  2. Rewrite right-side as expression
  %     * Listsigs are now reconstructions using lists:append and the various
  %       matched variables.
  %  3. IF there are non-right aggs, pair them up with the match terms to their
  %     right and use those pairs to generate the inner-call-chain.
  %     * Case statement for the listsig
  %     * "match" pattern+expression(from #2) as a case clause
  %     * Other clause that returns the original term.
  %  4. Function chain-link for each pair, that iterates, aggregates, and tries
  %     to match the upcoming "real" match.

  % TODO: special case listsig with only one _agg_ param inside.

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

atom(A) when is_atom(A) -> A;
atom(L) when is_list(L) -> list_to_atom(L).
