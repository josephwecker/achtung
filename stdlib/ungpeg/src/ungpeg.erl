-module(ungpeg).
-export([file/1, parse/1, optimize/1]).


%--------------------------------------- TOPLEVEL ----------------------------
file(FName) -> optimize(ungpeg_n:file(FName)).
parse(Txt) when is_list(Txt) -> optimize(ungpeg_n:parse(Txt)).

optimize([]) ->
  error_logger:error_msg("Empty grammar- parse it yourself.",[]);
optimize([{rule,First,_,_}|_] = AST) ->
  % 1. Turn it into a dictionary
  {Defs, EntryPoints} = make_defs(AST, dict:new(), [First]),
  % 2. Parse the parse-transformation functions & tag parts
  Defs2 = Defs:map(fun process_transformers/2),
  %Defs2 = Defs,
  % 3. Scan for recursive rules & entry-points
  TopLevels = scan_tls(EntryPoints, Defs2),
  % 4. Warn about any unused productions

  % 5. Inline everything that can be, unalias, and minor transforms
  Defs3 = tr_inline_all(TopLevels:to_list(), TopLevels, Defs2, dict:new()),
  % 6. Combine character classes wherever appropriate
  Defs4 = ast_map(Defs3, fun tr_combine_char/1),
  % 7. 

  Defs4:to_list();

optimize(AST) -> error_logger:error_msg("AST doesn't look well:~n  ~p~n~n",[AST]).

% Some general utilities for mapping and folding the ast/expression
ast_map(Defs,Fun) ->
  Defs:map(fun(_Name,Expr)-> expr_map(Expr, Fun) end).

expr_map(Expr, Fun) ->
  Expr2 = Fun(Expr),
  case Expr2 of
    {ord,Attr,L} -> {ord,Attr,[expr_map(L2,Fun)||L2<-L]};
    {seq,Attr,L} -> {seq,Attr,[expr_map(L2,Fun)||L2<-L]};
    _ -> Expr2
  end.

expr_mapfold(Fun, Acc, Expr) ->
  {Expr2, Acc2} = Fun(Expr, Acc),
  case Expr2 of
    {ord,Attr,L} ->
      {Expr3,Acc3}=lists:mapfoldl(fun(LExp,AccIn)->expr_mapfold(Fun,AccIn,LExp) end,Acc2,L),
      {{ord,Attr,Expr3}, Acc3};
    {seq,Attr,L} ->
      {Expr3,Acc3}=lists:mapfoldl(fun(LExp,AccIn)->expr_mapfold(Fun,AccIn,LExp) end,Acc2,L),
      {{seq,Attr,Expr3}, Acc3};
    _ -> {Expr2, Acc2}
  end.

%--------------------------------------- SETUP -------------------------------
%% Create general purpose lookup dictionary for the rules
make_defs([],Defs,EPs) -> {Defs,EPs};
make_defs([{rule,Name,Attrs,Expr}|R],Defs,EPs) ->
  EPs2 = case lists:member(entry,Attrs) of
    true -> [Name|EPs];
    false -> EPs
  end,
  make_defs(R,Defs:store(Name,{Attrs,Expr}),EPs2).

% Make sure all tags are where they need to be etc. so that elements can be
% found again after transformations occur.
% TODO:
%   - Deep search so you can tag stuff further down in the rules
%   - Warn when capturing things not normally captured (literals, notp, etc.)
process_transformers(Name,{Attrs,Expr}) ->
  case proplists:lookup(trans,Attrs) of
    none -> {Attrs,Expr};
    {_,TransExpr} ->
      Captures = erl_syntax_lib:fold(
        fun
          ({tree,atom,_,N},Acc) ->
            case atom_to_list(N) of
              [$$|V] ->
                case string:to_integer(V) of
                  {error,_} -> [list_to_atom(V)|Acc];
                  {Num,[]}  -> [Num|Acc];
                  _         -> [list_to_atom(V)|Acc]
                end;
              _ -> Acc
            end;
          (_,Acc) -> Acc
        end, [], erl_syntax:abstract(TransExpr)),
      {Attrs, verify_and_tag(Captures, Expr, Name)}
  end.

%% Verify that those captured variables exist and tag the appropriate
%% expressions in case things get reorganized.
verify_and_tag([],Expr,_Name) -> Expr;
% $n when there aren't that many parts
verify_and_tag([N|_],{_,_,[_|_]=L},Name) when is_number(N) and (N > length(L)) ->
  error_logger:error_msg("There is nothing at position $~p for rule ~p",[N,Name]);
% $0 or $_ --> whole thing
verify_and_tag([N|R],Expr,Name) when (N==0) or (N=='_')->
  verify_and_tag(R,tag_expr(Expr,N,Name),Name);
% Position based capture
verify_and_tag([N|R],{Type,Attr,[_|_]=L},Name) when is_number(N) and (N =< length(L)) ->
  {Before, [Change|After]} = lists:split(N-1,L),
  TaggedExp = tag_expr(Change,N,Name),
  verify_and_tag(R,{Type,Attr,Before++[TaggedExp]++After},Name);
% When $1 acts like $_ because there's only one part
verify_and_tag([1|R],Expr,Name) ->
  verify_and_tag(R,tag_expr(Expr,1,Name),Name);
% Named captures
verify_and_tag([A|R],Expr,Name) when is_atom(A) ->
  Res = expr_mapfold(fun(E,Acc) ->
        case matches_capture(A,E) of
          false -> {E, Acc};
          true -> {tag_expr(E, A, Name), Acc+1}
        end
    end, 0, Expr),
  {TaggedExprs, Found} = Res,
  case Found of
    0 -> error_logger:error_msg("There is no $~p for rule ~p.~n",[A,Name]);
    _ -> ok
  end,
  verify_and_tag(R,TaggedExprs,Name).

tag_expr({Type,Attr,Expr},Tag,Name) -> {Type,[{tag,{Tag,Name}}|Attr],Expr}.

matches_capture(Tag,{call,_,Tag}) -> true;
matches_capture(Special,{special,_,Special}) -> true;
matches_capture(Tag,{_,Attr,_}) ->
  lists:member({orig_tag,Tag},Attr) or
  lists:member({orig,Tag},Attr).


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

