-module(achtung_compile_rewrites).
-export([file/1,parse/2]).

-define(POS,{{line,Pos},_}).
-define(R(T),lists:reverse(T)).

file(FName) ->
  crewrites(achtung_rewrite_n:file(FName),module_from_filename(FName),FName).
parse(Txt,ModuleName) when is_list(Txt) ->
  crewrites(achtung_rewrite_n:parse(Txt),ModuleName,"").

crewrites({ok, _, []},ModuleName,FName) -> no_forms;
crewrites({ok, _, AST},ModuleName,FName) ->
  AST2 = apply_aliases(AST),
  Forms = generate_forms(AST2,ModuleName,FName),
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
  {RWClauses, LSFuns} = generate_clauses(Clauses),
  F = {function,L,chain_atom(NameParts),1,RWClauses},
  generate_functions(R,LSFuns ++ [F|Acc]);
generate_functions([_|R],Acc) -> generate_functions(R,Acc).

generate_clauses(RWCs) -> generate_clauses(RWCs,[],[]).
generate_clauses([],CAcc,FAcc) -> {?R([catchall_clause()|CAcc]), ?R(FAcc)};
generate_clauses([{rwclause,?POS,Left,Qual,Right}|R], CAcc, FAcc) ->
  {Clause, LSFuns} = fclause(Left,Qual,Right,Pos),
  generate_clauses(R,[Clause|CAcc],LSFuns++FAcc).

% TODO: put qualifiers in innermost case statement instead of at the function
% level so everything is ready.

% TODO: figure out return mechanism / trigger mechanism for warnings / errors

catchall_clause() -> {clause,1,[{var,1,'T'}],[],[{var,1,'T'}]}.

fclause(Left,Qual,Right,Pos) ->
  % Left side (top-level)
  Left2 = enumerate_anons(mark_variables(Left)),
  {LeftPattern, LSigs} = normalize_left(Left2,[]),
  TLWhen = [{call,0,{atom,0,is_list},[{var,0,AVName}]} ||
    {AVName,_} <- LSigs, used(AVName,LeftPattern)],
  TLWhen2 = case TLWhen of [] -> []; WList -> [WList] end,

  % DEBUG
  io:format("LEFTPATTERN:~n~p~n~nLEFTSIGS:~n~p~n-----~n",[LeftPattern,LSigs]),

  % Right Side
  Right2 = enumerate_anons(Right),
  {RightExpr, _LSigs2} = normalize_right(hd(Right2), []),  % TEMPORARY
  %{NextFun, RightExpr} = normalize_right(Right, LSigs),

  % Body and helper functions for listsigs
  {TLW, Body, LFuns} = case LSigs of
    [] -> {Qual, RightExpr, []};
    _ ->
      {LSigs2, B} = generate_body(LSigs,RightExpr,Qual,Pos),
      LF = generate_lfuns(LSigs2,Pos),
      {TLWhen2, B, LF}
  end,

  {{clause,1,[{match,1,LeftPattern,{var,1,'Original'}}],TLW,[Body]}, LFuns}.

% TODO: nowarn on unused variables

mark_variables(T) ->
  {T2,_} = ast_mapfold(fun mark_variables/2, nil, T),
  T2.
mark_variables({var,P,Val},_) ->
  Val2 = list_to_atom(atom_to_list(Val)++"_ARW"),
  {{var,P,Val2},nil};
mark_variables(V,_) -> {V,nil}.

enumerate_anons(T) ->
  put(agg_count,1),
  {T2, _} = ast_mapfold(fun enumerate_anons/2, nil, T),
  T2.
enumerate_anons({agg_term,P,N},_) when is_atom(N) ->
  {{agg_term,P,{var,P,next_agg(N)}},nil};
enumerate_anons(V,_) -> {V,nil}.

normalize_left({atom,?POS,Val},Acc) ->      {{atom,Pos,Val}, Acc};
normalize_left(A,Acc) when is_atom(A) ->    {{atom,0,A},Acc};
normalize_left({lsig,?POS,[]},Acc) ->       {{nil,Pos},Acc};
normalize_left([],Acc) ->                   {{nil,0},Acc};
normalize_left({match_term,_P,V},Acc) ->    normalize_left(V,Acc);
normalize_left({agg_term,_P,V},Acc) ->      normalize_left(V,Acc);
normalize_left({var,?POS,Val},Acc) ->       {{var,Pos,Val}, Acc};
normalize_left({tuple,?POS,L},Acc) ->
  {Pattern, Acc2} = lists:mapfoldl(fun normalize_left/2, Acc, L),
  {{tuple,Pos,Pattern}, Acc2};
normalize_left({lsig,?POS,L},Acc) ->
  {H,T} = lists:splitwith(fun({match_term,_,_})->true;(_)->false end, L),
  {L2,Acc2} = lists:mapfoldl(fun normalize_left/2, Acc, H),
  case T of
    [] -> {conses(L2,Pos), Acc2};
    [T1] ->
      {T2,Acc3} = normalize_left(T1,Acc2),
      {conses(L2,Pos,T2), Acc3};
    [T1|_TR]=TList ->
      {{var,P2,VarName}, Acc3} = normalize_left(T1,Acc2),
      VarName2 = list_to_atom("L" ++ atom_to_list(VarName)),
      {conses(L2,Pos,{var,P2,VarName2}), [{VarName2, TList}|Acc3]}
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



generate_body(LSigs,Right,Qual,Pos) ->
  {Sigs2, {_,Body}} = lists:mapfoldl(fun lsig_case/2, {Pos, inner, Qual,
      Right}, LSigs),
  {Sigs2, Body}.

%lsig_case(LSig, Core) -> {NewLSig, NewCore}.
% (case (call (atom ..) [(var ..)])
%   [(clause [(atom nomatch)] [] [(var Original)])
%    (clause [(tuple lsig-tuple)] [] [NEXT])])
lsig_case({VName,Parts},{Pos,inner,Qual,Right}) ->
  FName = next_lfun(),
  {{FName,VName,Parts},
    {Pos,
      {'case',Pos,{call,Pos,{atom,Pos,FName},[{var,Pos,VName},{nil,Pos}]},
        [{clause,Pos,[agg_tuple(Parts,Pos)],Qual,[Right]},
          {clause,Pos,[{var,Pos,'_'}],[],[{var,Pos,'Original'}]}]}
    }
  };
lsig_case({VName,Parts},{Pos,Core}) ->
  FName = next_lfun(),
  {{FName,VName,Parts},
    {Pos,
      {'case',Pos,{call,Pos,{atom,Pos,FName},[{var,Pos,VName},{nil,Pos}]},
        [{clause,Pos,[{atom,Pos,nomatch}],[],[{var,Pos,'Original'}]},
          {clause,Pos,[agg_tuple(Parts,Pos)],[],[Core]}]}
    }
  }.


generate_lfuns(LSigs,Pos) -> generate_lfuns(LSigs,Pos,[]).
generate_lfuns([],_,Acc) -> lists:reverse(Acc);
generate_lfuns([{FName,VName,Parts}|R],P,Acc) ->
  % TODO:
  %  1st clause: length shortcircuit (if list isn't long enough to succeed,
  %    then say so right away)
  F = {function,P,FName,2,
    [{clause,P,[{var,P,VName},{nil,P}],[[{op,P,'<',{call,P,{atom,P,length},[{var,P,VName}]},{integer,P,mlen(Parts)}}]],[{atom,P,nomatch}]}]
  },

  generate_lfuns(R,P,[F|Acc]).

mlen(P) -> mlen(P,0).
mlen([],N) -> N;
mlen([{match_term,_,_}|R],N) -> mlen(R,N+1);
mlen([_|R],N) -> mlen(R,N).

agg_tuple(AggParts, Pos) -> agg_tuple(AggParts, Pos, []).
agg_tuple([],Pos,Acc) -> {tuple,Pos,lists:reverse(Acc)};
agg_tuple([{_,_,Part}|R], Pos, Acc) ->
  case normalize_left(Part,[]) of
    {Norm,[]} -> agg_tuple(R,Pos,[Norm|Acc]);
    {_Norm,[{_Name,InnerParts}]} ->
      agg_tuple(R,Pos,[agg_tuple(InnerParts,Pos)|Acc]);
    {Norm,LSigs} ->
      {Inners,_} = ast_mapfold(fun sub_parts/2,LSigs,Norm),
      agg_tuple(R,Pos,[Inners|Acc])
  end.
sub_parts({var,P,N},LSigs) ->
  {case proplists:lookup(N,LSigs) of
      none -> {var,P,N};
      {_,Parts} -> agg_tuple(Parts,P)
    end, LSigs};
sub_parts(O,LSigs) -> {O, LSigs}.


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

next_lfun() ->
  C = case get(lsfun_count) of undefined -> 0; V -> V end,
  put(lsfun_count, C + 1),
  list_to_atom("--arwlsig" ++ integer_to_list(C) ++ "--").


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
