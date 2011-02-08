-module(leftr2).
-compile(export_all).

%----------------------- Macros etc. ------------------------------------------
-record(s,{
    bin,
    idx,
    pos_line,
    pos_col,
    memo=ets:new(leftr_memo,[set,private]),
    last_val=none,
    acc=[]}).

-define(bin_match(Idx,Find),
  <<_:Idx/bytes,Find,_/bytes>>).

-define(forward(State,IdxInc,LineInc,ColInc,Val),
  State#s{idx=State#s.idx + IdxInc,
          pos_line=State#s.pos_line + LineInc,
          pos_col=State#s.pos_col + ColInc,
          acc=[Val | State#s.acc]}).
-define(state(StateName,IndexName),
  #s{idx=IndexName}=StateName).

-define(final(State),
  case Final of
    true -> {succ, State#s{last_val=lists:reverse(State#s.acc),acc=[]}};
    false -> {succ, State}
  end).
-define(final(State,App),
  case Final of
    true -> {succ, State#s{last_val=lists:reverse([App|State#s.acc]),acc=[]}};
    false -> {succ, State#s{acc=[App|State#s.acc]}}
  end).


%----------------------- Main functions ---------------------------------------


'Value'(?state(S,I),Final) ->
  case S#s.bin of
    ?bin_match(I,C) when (C>=$0) and (C=<$9) ->
      'Va_1'(?forward(S,1,0,1,C),Final);
    ?bin_match(I,$() ->
      case 'Value'(?forward(S,1,0,1,$(),false) of % TODO: change to Expr
        fail -> fail;
        {succ, ?state(S2,I2)} ->
          case S2#s.bin of
            ?bin_match(I2,$)) -> ?final(S2,$));
            _ -> fail
          end
      end
  end.
    
'Va_1'(#s{idx=I}=S, Final) ->
  case S#s.bin of
    ?bin_match(I,C) when (C>=$0) and (C=<$9) ->
      'Va_1'(?forward(S,1,0,1,C),Final);
    _ -> ?final(S)
  end.

%'Sum'(St) ->
%  case 'Value'(St,true) of   % TODO: change to Expr




