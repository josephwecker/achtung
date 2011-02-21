-module(memo).
-export([memo/2]).

memo(Fun,Args) ->
  {name,   FunName} = erlang:fun_info(Fun,name),
  {module, ModName} = erlang:fun_info(Fun,module),
  {arity,  FunArity}= erlang:fun_info(Fun,arity),
  TableN = list_to_atom(lists:flatten(["--memoize--",
        atom_to_list(FunName),
        integer_to_list(FunArity),
        atom_to_list(ModName)])),
  Table = case ets:info(TableN,name) of
    undefined ->
      ets:new(TableN,[set,public,named_table,{read_concurrency,true}]);
    _ -> TableN
  end,
  case ets:lookup(Table,Args) of
    [] ->
      Res = apply(Fun,Args),
      ets:insert(Table,{Args,Res}),
      Res;
    [{_,Res}] -> Res
  end.
