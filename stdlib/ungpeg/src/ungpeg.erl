-module(ungpeg).
-export([file/1, parse/1, optimize/1]).


%--------------------------------------- TOPLEVEL ----------------------------
file(FName) -> optimize(ungpeg_n:file(FName)).
parse(Txt) when is_list(Txt) -> optimize(ungpeg_n:parse(Txt)).

optimize([]) ->
  error_logger:error_msg("Empty grammar- parse it yourself.",[]);
optimize([{rule,First,_,_}|_] = AST) ->
  % 1. Turn it into a dictionary
  {Defs, Entrances} = make_defs(AST, dict:new(), [First]),
  % 2. Parse the parse-transformation functions & tag parts
  Defs2 = Defs:map(fun process_transformers/2),
  % 3. Scan for recursive rules & entry-points
  TopLevelNames = get_toplevels(Entrances, Defs2),
  % 4. Warn about any unused productions

  % 5. Transform: Inline everything that can be (unalias)
  MainExprs = TopLevelNames:map(
    fun(TName,true)-> expr_map(Defs2:fetch(TName),
          fun(ExprIn)->tr_inline(ExprIn,TopLevelNames,Defs2) end) end),
  % 6. Tranform: Simple literals into character-classes
  Main2 = ast_map(MainExprs, fun tr_simple_lit_to_char/1),
  % 7. Combine character classes wherever appropriate
  Main3 = ast_map(Main2, fun tr_combine_char/1),

  Main3:to_list();

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
make_defs([{rule,Name,Attrs,{ExType,ExAttr,ExBody}}|R],Defs,EPs) ->
  EPs2 = case lists:member(entry,Attrs) of
    true -> [Name|EPs];
    false -> EPs
  end,
  make_defs(R,Defs:store(Name,{ExType,ExAttr++Attrs,ExBody}),EPs2).

% Make sure all tags are where they need to be etc. so that elements can be
% found again after transformations occur.
% TODO:
%   - Deep search so you can tag stuff further down in the rules
%   - Warn when capturing things not normally captured (literals, notp, etc.)
process_transformers(Name,{_ExType,ExAttr,_ExBody}=Expr) ->
  case proplists:lookup(trans,ExAttr) of
    none -> Expr;
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
      verify_and_tag(Captures, Expr, Name)
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
% need to be entrypoints as well).  Don't do multis yet- they'll be separated
% out later after some transformations.
get_toplevels(EPs,Defs) -> get_toplevels(EPs,Defs,dict:new()).
get_toplevels([],_Defs,Tops) -> Tops;
get_toplevels([EP|R],Defs,Tops) ->
  Body = Defs:fetch(EP),
  Tops2 = Tops:store(EP,true),
  Tops3 = get_tl_inner(Defs,Body,[EP],Tops2),
  get_toplevels(R,Defs,Tops3).

get_tl_inner(Defs,{call,_,Name},Seen,Tops) ->
  case Tops:is_key(Name) of
    true -> Tops;
    false -> case lists:member(Name,Seen) of
        true -> Tops:store(Name,true);
        false ->
          case Defs:find(Name) of
            error -> error_logger:error_msg("Couldn't find a definition for ~p",[Name]);
            {ok, Body} -> get_tl_inner(Defs,Body,[Name|Seen],Tops)
          end
      end
  end;
get_tl_inner(Defs,{_,_,Exprs},Seen,Tops) when is_list(Exprs) ->
  lists:foldl(fun(Body,OldTops) -> get_tl_inner(Defs,Body,Seen,OldTops) end,
    Tops, Exprs);
get_tl_inner(_,_,_,Tops) -> Tops.

%--------------------------------------- TRANSFORMATIONS ---------------------

% Multi-level unalias
tr_inline({call,CallAttrs,Name}=Call, TLNames, Defs) ->
  case TLNames:is_key(Name) of
    true -> Call;
    false ->
      {ExprType, ExprAttrs, ExprBody} = Defs:fetch(Name),
      tr_inline({ExprType,ExprAttrs++CallAttrs,ExprBody},TLNames,Defs)
  end;
tr_inline(Expr, _TLNames, _Defs) -> Expr.

% Turn literals that only have one char into character class (in case it can,
% in turn, be combined)
tr_simple_lit_to_char({lit,Attrs,<<Chr>>}) -> {char,Attrs,[Chr]};
tr_simple_lit_to_char(Other) -> Other.

% Combine ord-charclasses into charclass
tr_combine_char({ord,Attr,OrdList}) -> {ord, Attr, tr_combine_char_inner(OrdList,[])};
tr_combine_char(Other) -> Other.
tr_combine_char_inner([],Acc) -> lists:reverse(Acc);
tr_combine_char_inner([One],Acc) -> lists:reverse([One|Acc]);
tr_combine_char_inner([{char,A1,Ranges1}|[{char,A2,Ranges2}|R]],Acc) when
  ((A1 == []) or (A1 == [orig])) and ((A2 == []) or (A2 == [orig])) ->
    tr_combine_char_inner([{char,A1++A2,lists:usort(Ranges1++Ranges2)}|R], Acc);
tr_combine_char_inner([Other|R],Acc) -> tr_combine_char_inner(R,[Other|Acc]).

% - combine seq-single-range-charclasses into literals

