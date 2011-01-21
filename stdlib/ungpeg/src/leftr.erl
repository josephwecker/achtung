-module(leftr).
-compile(export_all).
% Expr    ← Product / Sum / Value
% Product ← Expr (([*] / [/]) Expr)*
% Sum     ← Expr (([+] / [-]) Expr)*
% Value   ← [0-9]* / [(] Expr [)]
%
% Expr    <- Product / Sum / Value
% Product <- Expr Pr_1
% Sum     <- Expr Su_1
% Value   <- Va_1 / [(] Expr [)]
% Pr_1    <- ([*]/[/]) Expr Pr_1 / succ
% Su_1    <- ([+]/[-]) Expr Su_1 / succ
% Va_1    <- [0-9] Va_1 / succ
%
% Expr_sp <- (Product / Sum / Value / Expr_end) Expr_agg
%
% Only Expr marked as recursive so only it has memo lookup etc.
% 



% Va_1    <- [0-9] Va_1 / succ
'Va_1'({Bin,Idx},{Line,Col}=Pos,S,A) ->
  case Bin of
    <<_:Idx/bytes,C,_/bytes>> when (C >= $0) and (C =< $9) -> 'Va_1'({Bin,Idx+byte_size(C)},{Line,Col+1},S,[C|A]);
    _ -> {succ, {Bin, Idx}, Pos, S, lists:reverse(A)}
  end.

% Su_1    <- ([+];[-]) Expr / succ
'Su_1'({Bin,Idx},{Line,Col}=Pos,S,A) ->
  case Bin of
    <<_:Idx/bytes,$+,_/bytes>> -> 'Su_1'({Bin,Idx+1},{Line,Col+1},S,[$+|A]);
    <<_:Idx/bytes,$-,_/bytes>> -> 'Su_1'({Bin,Idx+1},{Line,Col+1},S,[$-|A])



'Expr'(Inp,Pos,S) ->
  % Look on stack
  % (If old position: val: first, remove it.)
  % If nothing:
  %     - New entry at current position value: first
  %     - Evaluate normally
  % If curr-pos & val=first: 
  %     - Change val to: recurse,
  %     - Evaluate the fail version which ends in 'special_expr'
  % 
  %             
  





-record(pstate, {
    memo
  }).

-define(seen(Name),
  hd(ets:lookup(S#pstate.memo, {Name,Idx}))
).

% Templated entry-point
'expr'(Input) when is_list(Input) -> 'expr'(iolist_to_binary(Input));
'expr'(InputBin) ->
  Memo = ets:new(leftr_memotable,[set, private]),
  'expr'({InputBin,0},{0,0},#pstate{memo=Memo}).

'expr'({Bin,Idx}=Inp,Pos,S) ->
  case ?seen('expr') of



'num'({Bin,Idx},{Line,Col}=Pos,S) ->
  case Bin of
    <<_:Idx/bytes,C,_/bytes>> when (C >= $0) and (C =< $9) ->
      {succ, {_,Idx2},Pos2,S2,Rest} = 'num_2'({Bin,Idx+1},{Line,Col+1},S),
      {succ, {Bin,Idx2},Pos2,S2,[C|Rest]};
    <<_:Idx/bytes,C,_/bytes>> -> {fail, Pos, {unexpected, C, S#pstate.stack}};
    <<>> -> {fail, Pos, {unexpected, eof, 'num'}}
  end.

'num_2'(Inp,Pos,S) -> 'num_2'(Inp,Pos,S,[]).
'num_2'({Bin,Idx},{Line,Col}=Pos,S,Acc) ->
  case Bin of
    <<_:Idx/bytes,C,_/bytes>> when (C >= $0) and (C =< $9) ->
      'num_2'({Bin,Idx+1},{Line,Col+1},S,[C|Acc]);
    _ -> {succ, {Bin,Idx},Pos,S,lists:reverse(Acc)}
  end.



% 1: expr <- expr [/] [0-9] / [0-9]

%'expr'(true,Inp,Pos,S) ->
%  {Seed,Inp2,Pos2} = 'expr'(false,Inp,Pos,S),
%  'expr'(Seed,Inp2,Pos2);
%'expr'(false,Inp,Pos,S) -> {[0],Inp,Pos,S};
%'expr'(Expr,Inp,Pos,S) ->





