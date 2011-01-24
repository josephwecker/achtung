-module(ungpeg_optimize).
-export([file/1, parse/1, pretty_print/1, pretty_print_expr/1,
    pretty_print_expr/2]).

-record(compile, {
    options =[],
    debugs  =[],
    infos   =[],
    warnings=[],
    errors  =[]}).

%--------------------------------------- TOPLEVEL ----------------------------
file(FName) -> optimize(ungpeg_n:file(FName), #compile{}).
parse(Txt) when is_list(Txt) ->
  FinalState = optimize(ungpeg_n:parse(Txt), #compile{}),
  FinalState.

optimize([], _State) ->
  error_logger:error_msg("Empty grammar- parse it yourself.", []);
optimize([{rule,First,_,_}|_] = AST, _State) ->
  % 1. Turn it into a dictionary
  {Defs, Entrances} = make_defs(AST, dict:new(), [First]),
  % 2. Parse the parse-transformation functions & tag parts
  Defs2 = Defs:map(fun process_transformers/2),
  % 3. Scan for recursive rules & entry-points
  TopLevelNames = get_toplevels(Entrances, Defs2),
  % 4. TODO: Warn about any unused productions

  % 5. Transform: Inline everything that can be (unalias)
  MainExprs = TopLevelNames:map(
    fun(TName,true)-> expr_map(Defs2:fetch(TName),
          fun(ExprIn)->tr_inline(ExprIn,TopLevelNames,Defs2) end) end),
  % 6. Rest of transformations
  MainExprs2 = all_transformations(MainExprs),
  ast_map(fun first_fixed/1, MainExprs2),

  MainExprs2:to_list();

optimize(AST, _State) -> error_logger:error_msg("AST doesn't look well:~n  ~p~n~n",[AST]).

all_transformations(Defs) ->
  lists:foldl(fun ast_map/2,
    Defs, [
      %fun tr_simple_lit_to_char/1,
      %fun tr_combine_char/1
      fun tr_lits_to_chars/1,
      fun tr_expand_chars/1,
      fun tr_expand_plusses/1    %% Now in 2nd form (assuming inlining has taken place)
      %fun tr_condense_ords_seqs/1
    ]).

% Some general utilities for mapping and folding the ast/expression.  We
% normalize attributes after every ast_map.
ast_map(Fun,Defs) ->
  normalize_attributes(Defs:map(fun(_Name,Expr)-> expr_map(Expr, Fun) end)).

normalize_attributes(Defs) -> Defs.
  %Defs:map(fun(_Name,Expr) -> expr_map(Expr, fun norm_attr/1) end).

expr_map(Expr, Fun) ->
  % TODO: flatten and remove []'s so that they can be expanded & shrunk
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

%--------------------------------------- ATTRIBUTE NORMALIZATION -------------
%-record(attr, {
%    notp=false, andp=false,                   % Prefixes / predicates
%    star=false, plus=false, opt=false,        % Suffixes
%    token, trans, tag, orig, orig_tag, entry  % Parsing
%  }).
%norm_attr({Type,Attrs,Body}) ->
%  norm_attr_inner({Type,attr_normal_form(Attrs),Body}).
%attr_normal_form(A) -> attr_normal_form(A,#attr{}).
%anf([notp|R],A)->anf(R,A#attr{notp=not A#attr.notp, andp=true}); % Norm: !e1 == !&e1; !&(!&e1) == &e1
%anf([andp|R],A)->anf(R,A#attr{andp=true});
%anf([star|R],A#attr{plus=true})->anf(R,A#attr{plus=false,star=true});  % Plus is dropped because: (e1+)* == e1*
%anf([star|R],A)->anf(R,A#attr{star=true});
%anf([plus|R],A#attr{star=false})->anf(R,A#attr{plus=true});  % (e1*)+ will never succeed
%anf([plus|R],A)->throw("Expression never succeeds (child * eats everything so there's nothing left for +)");
%anf([opt |R],A#attr{star=true})->anf(R,A);
%anf([opt |R],A)->anf(R,A#attr{opt=true});
%anf([
%norm_attr(Expr) -> Expr.



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

%% Turn literals that only have one char into character class (in case it can,
%% in turn, be combined)
%% Before:  a <- 'a' 'bc'
%% After:   a <- [a] 'bc'
%tr_simple_lit_to_char({lit,Attrs,<<Chr>>}) -> {char,Attrs,[Chr]};
%tr_simple_lit_to_char(Other) -> Other.

%% Combine ord-charclasses into charclass. Captures no problem because we're in
%% an ord, but if any are prefixed or suffixed this transform doesn't apply.
%% Before:  a <- [a]/[b]/[c]
%% After:   a <- [abc]
%tr_combine_char({ord,Attr,OrdList})->{ord,Attr,tr_combine_char_inner(OrdList,[])};
%tr_combine_char(Other) -> Other.
%tr_combine_char_inner([],Acc) -> lists:reverse(Acc);
%tr_combine_char_inner([One],Acc) -> lists:reverse([One|Acc]);
%tr_combine_char_inner([{char,A1,Ranges1}|[{char,A2,Ranges2}|R]],Acc) ->
%  case {A1,A2} of
%    {[],[]} -> 
%  ((A1 == []) or (A1 == [orig])) and ((A2 == []) or (A2 == [orig])) ->
%    tr_combine_char_inner([{char,A1++A2,lists:usort(Ranges1++Ranges2)}|R], Acc);
%tr_combine_char_inner([Other|R],Acc) -> tr_combine_char_inner(R,[Other|Acc]).

% Sequential literals or 1-char charclasses combine
% Before:  a <- 'hey' [\t] ':'
% After:   a <- 'hey\t:'

%tr_combine_lits({seq,Attr,EList})->{seq,Attr,tr_combine_lits_inner(EList,[])};

tr_lits_to_chars({lit,A,<<Char>>}) ->{char,A,[Char]};
tr_lits_to_chars({lit,A,Chars})->{seq,A,[{char,[],[C]}||C<-binary_to_list(Chars)]};
tr_lits_to_chars(Other)->Other.

tr_expand_chars({char,_,[_]}=E)-> E;
tr_expand_chars({char,Attrs,Ranges})->{ord,Attrs,[{char,[],[R]}||R<-Ranges]};
tr_expand_chars(Other)->Other.

tr_expand_plusses({Type,Attrs,Body}=E)->
  case lists:member(plus,Attrs) of
    true -> {seq,lists:delete(plus,Attrs),[{Type,[],Body},{Type,[star],Body}]};
    false -> E
  end.

%--------------------------------------- UTILITIES ---------------------------

first_fixed({_,Attrs,_}=E) ->
  case lists:member(star,Attrs) of
    true -> none;
    false ->
      case lists:member(opt,Attrs) of
        true -> none;
        false -> first_fixed_i(E)
      end
  end.
first_fixed_i({char,_,[R]}) -> R;
first_fixed_i({ord,A,Exprs}) ->
  R = common_prefix([first_fixed(E)||E<-Exprs]),
  %io:format("Common prefix for ~p: <~s>~n",
  %  [{ord,A},lists:map(fun({C1,C2})->[$[,C1,$-,C2,$]];(C3)->C3 end, R)]),
  R;
first_fixed_i({seq,A,Exprs}) ->
  R = common_prefix([lists:flatten([first_fixed(E)||E<-Exprs])]),
  %io:format("Common prefix for ~p: <~s>~n",
  %  [{seq,A},lists:map(fun({C1,C2})->[$[,C1,$-,C2,$]];(C3)->C3 end, R)]),
  R;
first_fixed_i({special,_,S}) -> S;
first_fixed_i(_) -> none.

%% Given a list of lists, returns whatever prefix they all have in common.
common_prefix(Lists) -> common_prefix(Lists,[]).
common_prefix(Lists,Acc) ->
  case lists:member([],Lists) of
    true -> lists:reverse(Acc);
    false ->
      Firsts = [T||[T|_]<-Lists],
      Rests = [R||[_|R]<-Lists],
      case lists:usort(Firsts) of
        [none] -> lists:reverse(Acc);
        []     -> lists:reverse(Acc);
        [Term] -> common_prefix(Rests, [Term|Acc]);
        _      -> lists:reverse(Acc)
      end
  end.


pretty_print(RuleList) -> pretty_print(RuleList, []).
pretty_print([], Acc)  -> io:format("~s~n~n", [string:join(lists:reverse(Acc),"\n")]);
pretty_print([{Name,Expr}|R], Acc) ->
  pretty_print(R, [[atom_to_list(Name), " <- ", pretty_print_expr(Expr,1)] | Acc]).

% ord within seq always parenth.
% ord within ord always parenth.
% seq within seq always parenth.
% space if all children !atomic

pretty_print_expr(Expr)     -> pretty_print_expr(Expr,1,none).
pretty_print_expr(Expr,Lvl) -> pretty_print_expr(Expr,Lvl,none).

pretty_print_expr({char,Attr,Ranges},_Lvl,_Parent) ->
  [pp_prefs(Attr),"[",lists:map(fun({C1,C2})->esc_str([C1,$-,C2]);(Ch)->esc_str(Ch) end,Ranges),"]",pp_suffs(Attr)];
pretty_print_expr({lit,Attr,Bin},_Lvl,_Parent) ->
  [pp_prefs(Attr),"'",chr_replace($',"\\'",esc_str(binary_to_list(Bin))),"'",pp_suffs(Attr)];
pretty_print_expr({call,Attr,Name},_Lvl,_Parent) ->
  [pp_prefs(Attr),atom_to_list(Name),pp_suffs(Attr)];
pretty_print_expr({special,Attr,Type},_Lvl,_Parent) ->
  [pp_prefs(Attr),atom_to_list(Type),pp_suffs(Attr)];
pretty_print_expr({ord,Attr,Exprs},Lvl,Parent) ->
  Prefs = pp_prefs(Attr),
  Suffs = pp_suffs(Attr),
  Paren  = (length(Prefs++Suffs) > 0) or (Parent == seq) or (Parent == ord),
  {Begin,End} = case Paren of false->{[],[]};_->{[Prefs,$(],[$),Suffs]} end,
  case all_atomics(Exprs) of
    true -> [Begin,string:join([pretty_print_expr(E,Lvl+1,ord)||E<-Exprs],"/"),End];
    false-> [Begin,string:join([pretty_print_expr(E,Lvl+1,ord)||E<-Exprs]," / "),End]
  end;
pretty_print_expr({seq,Attr,Exprs},Lvl,Parent) ->
  Prefs = pp_prefs(Attr),
  Suffs = pp_suffs(Attr),
  Paren  = (length(Prefs++Suffs) > 0) or (Parent == seq),
  {Begin,End} = case Paren of false->{[],[]};_->{[Prefs,$(],[$),Suffs]} end,
  [Begin,string:join([pretty_print_expr(E,Lvl+1,seq)||E<-Exprs]," "),End].

pp_prefs(Attr)                 -> pp_prefs(Attr,[]).
pp_prefs([],Acc)               -> lists:reverse(Acc);
pp_prefs([notp|R],Acc)         -> pp_prefs(R,[$!|Acc]);
pp_prefs([andp|R],Acc)         -> pp_prefs(R,[$&|Acc]);
pp_prefs([{orig_tag,T}|R],Acc) -> pp_prefs(R,[[atom_to_list(T)|":"]|Acc]);
pp_prefs([_|R],Acc)            -> pp_prefs(R,Acc).
pp_suffs(Attr)                 -> pp_suffs(Attr,[]).
pp_suffs([],Acc)               -> lists:reverse(Acc);
pp_suffs([star|R],Acc)         -> pp_suffs(R,[$*|Acc]);
pp_suffs([plus|R],Acc)         -> pp_suffs(R,[$+|Acc]);
pp_suffs([opt|R],Acc)          -> pp_suffs(R,[$?|Acc]);
pp_suffs([_|R],Acc)            -> pp_suffs(R,Acc).

all_atomics(L) ->
  lists:all(fun
      ({char,_,_})    -> true;
      ({lit,_,_})     -> true;
      ({special,_,_}) -> true;
      ({call,_,_})    -> true;
      ({ord,_,_})     -> false;
      ({seq,_,_})     -> false
    end, L).

chr_replace(Needle,Replacement,Haystack) ->
  chr_replace(Needle,Replacement,Haystack,[]).
chr_replace(_,_,[],A)   ->lists:flatten(lists:reverse(A));
chr_replace(C,R,[C|T],A)->chr_replace(C,R,T,[R|A]);
chr_replace(C,R,[H|T],A)->chr_replace(C,R,T,[H|A]).

esc_str(C) when is_integer(C) -> esc_str([C],[]);
esc_str(S) when is_list(S) -> esc_str(S,[]).
esc_str([],Acc) -> lists:reverse(Acc);
esc_str([$\t|R],Acc) -> esc_str(R,["\\t"|Acc]);
esc_str([$\n|R],Acc) -> esc_str(R,["\\n"|Acc]);
esc_str([$\r|R],Acc) -> esc_str(R,["\\r"|Acc]);
esc_str([N|R],Acc) when (N < $ ) or (N > 254) ->
  esc_str(R,[["\\x{",integer_to_list(N,16),"}"]|Acc]);
esc_str([C|R],Acc) -> esc_str(R,[C|Acc]).
