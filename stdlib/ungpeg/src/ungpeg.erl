-module(ungpeg).
-export([file/1, parse/1, optimize/1]).


file(FName) -> optimize(ungpeg_n:file(FName)).
parse(Txt) when is_list(Txt) -> optimize(ungpeg_n:parse(Txt)).

optimize([]) ->
  error_logger:error_msg("Empty grammar- parse it yourself.",[]);
optimize([{rule,First,_,_}|_] = AST) ->
  % 1. Turn it into a dictionary
  {Defs, EntryPoints} = make_defs(AST, dict:new(), [First]),
  % 2. Parse the parse-transformation functions & tag parts
  %Defs2 = ast_map(Defs, fun process_transformers/1),
  Defs2 = Defs,
  % 3. Scan for recursive rules & entry-points
  TopLevels = scan_tls(EntryPoints, Defs2),
  % 4. Warn about any unused productions

  % 5. Inline everything that can be, unalias, and minor transforms
  Defs3 = tr_inline_all(TopLevels:to_list(), TopLevels, Defs2, dict:new()),
  % 6. Combine character classes wherever appropriate
  Defs4 = ast_map(Defs3, fun tr_combine_char/1),
  % 7. 

  Defs4:to_list();

optimize(AST) -> error_logger:error_msg("AST doesn't look well.~n~p",[AST]).

% Create general purpose lookup dictionary for the rules

make_defs([],Defs,EPs) -> {Defs,EPs};
make_defs([{rule,Name,Attrs,Expr}|R],Defs,EPs) ->
  EPs2 = case lists:member(entry,Attrs) of
    true -> [Name|EPs];
    false -> EPs
  end,
  make_defs(R,Defs:store(Name,{Attrs,Expr}),EPs2).

% Add entrypoints to toplevel and scan rules for recursion (which implies they
% need to be entrypoints as well).
scan_tls(EPs,Defs) -> scan_tls(EPs,Defs,dict:new()).
scan_tls([],_Defs,Tops) -> Tops;
scan_tls([EP|R],Defs,Tops) ->
  {_Attrs,Body} = Defs:fetch(EP),
  Tops2 = Tops:store(EP,true),
  Tops3 = scan_for_tops(Defs,Body,[],Tops2),
  scan_tls(R,Defs,Tops3).

scan_for_tops(Defs,{call,_,Name},Seen,Tops) ->
  case dict:is_key(Name,Tops) of
    true -> Tops;
    false -> case lists:member(Name,Seen) of
        true -> Tops:store(Name,true);
        false ->
          case Defs:find(Name) of
            error -> error_logger:error_msg("Couldn't find a definition for ~p",[Name]);
            {ok, {_,Body}} -> scan_for_tops(Defs,Body,[Name|Seen],Tops)
          end
      end
  end;
scan_for_tops(Defs,{_,_,Exprs},Seen,Tops) when is_list(Exprs) ->
  lists:foldl(fun(Body,OldTops) -> scan_for_tops(Defs,Body,Seen,OldTops) end,
    Tops, Exprs);
scan_for_tops(_,_,_,Tops) -> Tops.

ast_map(Defs,Fun) ->
  Defs:map(fun(_Name,Expr)-> ast_map_inner(Expr, Fun) end).
ast_map_inner(Expr, Fun) ->
  Expr2 = Fun(Expr),
  case Expr2 of
    {ord,Attr,L} -> {ord,Attr,[ast_map_inner(L2,Fun)||L2<-L]};
    {seq,Attr,L} -> {seq,Attr,[ast_map_inner(L2,Fun)||L2<-L]};
    _ -> Expr2
  end.

%--------------------------------------- TRANSFORMATIONS ---------------------

% - TODO: Tag all parts first for transformations
% Inlining performs the following transformations explicitly or implicitly:
% - multi-level unalias
% - single-literals to charclass
% - normalize attributes (TODO)
%   - reduce
%   - remove duplicates
%
tr_inline_all([],_TLevs,_Defs,NewDefs) -> NewDefs;
tr_inline_all([{EP,true}|R],TLevs,Defs,NewDefs) ->
  {RuleAttrs,Expr} = Defs:fetch(EP),
  {InnerType, InnerAttr, InnerBody} = tr_inline_parts(Expr,TLevs,Defs),
  % Attributes in order- apply from left to right
  tr_inline_all(R,TLevs,Defs,
    NewDefs:store(EP,{InnerType,InnerAttr++RuleAttrs,InnerBody})).

tr_inline_parts({lit,Attrs,[Chr]},_TLevs,_Defs) -> {char,Attrs,[Chr]};
tr_inline_parts({Type,Attrs,Exprs},TLevs,Defs) when is_list(Exprs) ->
  {Type,Attrs,[tr_inline_parts(E,TLevs,Defs)||E<-Exprs]};
tr_inline_parts({call,CallAttrs,Name},TLevs,Defs) ->
  case TLevs:find(Name) of
    {ok,_} -> {call,CallAttrs,Name};
    _ ->
      {RuleAttrs, {ExprType,RuleTopExprAttrs,EBody}} = Defs:fetch(Name),
      % Attributes in order- apply from left to right
      tr_inline_parts({ExprType,RuleTopExprAttrs ++ RuleAttrs ++ CallAttrs, EBody},TLevs,Defs)
  end;
tr_inline_parts(Atomic,_TLevs,_Defs) -> Atomic.


% - combine ord-charclasses into charclass
tr_combine_char({ord,Attr,OrdList}) -> {ord, Attr, tr_combine_char_inner(OrdList,[])};
tr_combine_char(Other) -> Other.
tr_combine_char_inner([],Acc) -> lists:reverse(Acc);
tr_combine_char_inner([One],Acc) -> lists:reverse([One|Acc]);
tr_combine_char_inner([{char,A1,Ranges1}|[{char,A2,Ranges2}|R]],Acc) when
  ((A1 == []) or (A1 == [orig])) and ((A2 == []) or (A2 == [orig])) ->
    tr_combine_char_inner([{char,A1++A2,lists:usort(Ranges1++Ranges2)}|R], Acc);
tr_combine_char_inner([Other|R],Acc) -> tr_combine_char_inner(R,[Other|Acc]).

% - combine seq-single-range-charclasses into literals

