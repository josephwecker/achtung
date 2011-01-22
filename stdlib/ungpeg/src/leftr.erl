-module(leftr).
-compile(export_all).
% Expr    ← Product / Sum / Value     -> {expr $_}
% Product ← Expr (([*] / [/]) Expr)*  -> [Expr | $2]
% Sum     ← Expr (([+] / [-]) Expr)*  -> [Expr | $2]
% Value   ← [0-9]+ / [(] Expr [)]     -> $1 / Expr
%
% Expr    <- Product / Sum / Value
% Product <- Expr Pr_1
% Sum     <- Expr Su_1
% Value   <- [0-9] Va_1 / [(] Expr [)]
% Pr_1    <- ([*]/[/]) Expr Pr_1 / succ
% Su_1    <- ([+]/[-]) Expr Su_1 / succ
% Va_1    <- [0-9] Va_1 / succ
%
% Expr_sp <- (Product / Sum / Value / Expr_end) Expr_agg
%
% Only Expr marked as recursive so only it has memo lookup etc.
% 


parse(Txt) ->
  R = 'Expr'({list_to_binary(Txt),
      0, 0, 0,
      ets:new(leftr_memo, [set, private]),
      []}),
  case R of
    fail -> fail;
    {succ, {_, _, _, _, M, A}} ->
      ets:delete(M),
      lists:reverse(A)
  end.


'Expr'({Bin,I,PL,PC,M,A}=St) ->
  Key = 'Expr',
  case ets:lookup(M,Key) of
    [{_,I,lr,fail}] -> fail;
    [{_,I,lr,Val}] -> {succ,{Bin,I,PL,PC,M,[Val|A]}};
    Cont ->
      case Cont of
        [{_,I,first}] ->
          ets:insert(M,{Key,I,lr,fail}),
          'Expr_sp'(St);
        _ ->
          ets:insert(M,{Key,I,first}),
          case 'Product'(St) of
            {succ,Succ} -> {succ,Succ};
            fail ->
              case 'Sum'(St) of
                {succ,Succ} -> {succ,Succ};
                fail ->
                  case 'Value'(St) of
                    {succ,Succ} -> {succ,Succ};
                    fail -> fail
                  end
              end
          end
      end
  end.

% Expr_sp <- (Product / Sum / Value / Expr_end) Expr_agg
'Expr_sp'(St) ->
  case
    case 'Product'(St) of
      {succ,Succ} -> {succ,Succ};
      fail ->
        case 'Sum'(St) of
          {succ,Succ2} -> {succ,Succ2};
          fail ->
            case 'Value'(St) of
              {succ,Succ3} -> {succ,Succ3};
              fail -> fail
            end
        end
    end of
    fail -> 'Expr_end'(St);
    {succ,Succ4} -> 'Expr_agg'(Succ4)
  end.

'Expr_end'({Bin,I,PL,PC,M,A}) ->
  {succ,{Bin,I,PL,PC,M,A}}.

'Expr_agg'({Bin,I,PL,PC,M,A}) ->
  case ets:lookup(M,'Expr') of
    [{_,I,lr,_}] -> {succ,{Bin,I,PL,PC,M,A}};
    _ ->
      ets:insert(M,{'Expr',I,lr,A}),
      'Expr_sp'({Bin,I,PL,PC,M,A})
  end.

% Product <- Expr Pr_1
'Product'(St) ->
  case 'Expr'(St) of
    {succ, St2} -> 'Pr_1'(St2);
    fail -> fail
  end.

% Sum     <- Expr Su_1
'Sum'(St) ->
  case 'Value'(St) of
    {succ, St2} -> 'Su_1'(St2);
    fail -> fail
  end.

% Value   <- [0-9] Va_1 ; [(] Expr [)]
'Value'(St) -> 'Value'(St,[]).
'Value'({Bin,I,PL,PC,Etc,A},IA) ->
  case Bin of
    <<_:I/bytes,C,_/bytes>> when (C >= $0) and (C =< $9) ->
      'Va_1'({Bin,I+1,PL,PC+1,Etc,A},[C|IA]);
    <<_:I/bytes,$(,_/bytes>> ->
      case 'Expr'({Bin,I+1,PL,PC+1,Etc,[$(|A]}) of
        fail -> fail;
        {succ, {Bin2,I2,PL2,PC2,Etc2,A2}} ->
          case Bin2 of
            <<_:I2/bytes,$),_/bytes>> ->
              {succ, {Bin2,I2+1,PL2,PC2+1,Etc2,[$)|A2]}};
            _ -> fail
          end
      end;
    _ -> fail
  end.

% Va_1    <- [0-9] Va_1 / succ
'Va_1'({Bin,I,PL,PC,Etc,A},IA) ->
  case Bin of
    <<_:I/bytes,C,_/bytes>> when (C >= $0) and (C =< $9) ->
      'Va_1'({Bin,I+1,PL,PC+1,Etc,A},[C|IA]);
    _ -> {succ, {Bin,I,PL,PC,Etc,[lists:reverse(IA)|A]}}
  end.

'Su_1'(St) -> 'Su_1'(St,[]).
'Su_1'({Bin,I,PL,PC,Etc,A}=St,IA) ->
  case
    case Bin of
      <<_:I/bytes,$+,_/bytes>> -> {succ, {Bin,I+1,PL,PC+1,Etc,A},[$+|IA]};
      <<_:I/bytes,$-,_/bytes>> -> {succ, {Bin,I+1,PL,PC+1,Etc,A},[$-|IA]};
      _ -> fail
    end
    of
    fail -> {succ,{Bin,I,PL,PC,Etc,[lists:reverse(IA)|A]}};
    {succ,{Bin2,I2,PL2,PC2,Etc2,A},IA2} ->
      case 'Value'({Bin2,I2,PL2,PC2,Etc2,IA2}) of
        fail -> {succ, {Bin,I,PL,PC,Etc,[lists:reverse(IA)|A]}};
        {succ,{B3,I3,PL3,PC3,E3,IA3}} ->
          'Su_1'({B3,I3,PL3,PC3,E3,A},IA3)
      end
  end.

'Pr_1'({Bin,I,PL,PC,Etc,A}=St) ->
  case
    case Bin of
      <<_:I/bytes,$*,_/bytes>> -> {succ, {Bin,I+1,PL,PC+1,Etc,[$*,A]}};
      <<_:I/bytes,$/,_/bytes>> -> {succ, {Bin,I+1,PL,PC+1,Etc,[$/,A]}};
      _ -> fail
    end
    of
    fail -> {succ, St};
    {succ,St2} ->
      case 'Expr'(St2) of
        fail -> {succ, St};
        {succ,St3} -> 'Pr_1'(St3)
      end
  end.
