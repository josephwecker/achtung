-module(func_recs).
-export([generate/1]).

generate(Filename) ->
  {ok, Forms} = epp:parse_file(Filename, [], []),
  lists:foreach(
    fun({attribute, _, record, {RecName, RecFields}}) ->
        case smerl:for_module(RecName) of
          {ok, C1} ->
            process_module(C1, RecFields);
          _Err ->
            process_module(smerl:new(RecName), RecFields)
        end;
      (_Other) -> undefined
    end, Forms).

process_module(MetaCtx, RecFields) ->
  {_, C2} = lists:foldl(
    fun({record_field, _, {atom, _, FieldName}}, {Idx, MetaCtx1}) ->
        {Idx+1, process_field(MetaCtx1, FieldName, Idx)};
      ({record_field, _, {atom, _, FieldName}, _}, {Idx, MetaCtx1}) ->
        {Idx+1, process_field(MetaCtx1, FieldName, Idx)}
    end, {2, MetaCtx}, RecFields),
  smerl:compile(C2).

get(Idx, Obj) ->
  element(Idx, Obj).

set(Idx, Obj, Val) ->
  setelement(Idx, Obj, Val).

process_field(MetaCtx, FieldName, Idx) ->
  {ok, Getter} =
  smerl:curry(func_recs, get, 2, Idx, FieldName),
  {ok, Setter} =
  smerl:curry(func_recs, set, 3, Idx, FieldName),
  {ok, MetaCtx1} = smerl:add_func(MetaCtx, Getter),
  {ok, MetaCtx2} = smerl:add_func(MetaCtx1, Setter),
  MetaCtx2.

