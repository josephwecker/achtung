-module(leftr).
-compile(export_all).
% Original
% 1: expr <- expr [/] num / num
% 2: num  <- [0-9]+

% First Normal
% expr  <- expr [/] num / num
% num   <-- [0-9] num_2
% num_2[orig:num] <-- [0-9] num_2 / succ


% left-recursion detected, run fail version

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





