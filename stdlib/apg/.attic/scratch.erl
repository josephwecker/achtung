-module(erlrat).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input  : Input, Pos, A
% Output : {false, Msg} | {true, NewIdx, NewA}

%  expr    <- _sp sum _sp
%  product <- value _sp (('*' / '/') _sp value)*
%  sum     <- product _sp (('+' / '-') _sp product)*
%  value   <- [0-9]+ / '(' expr ')'

%  numberlst<- _sp ([0-9]+ _sp)+
% D _sp     <- sp*
% D sp      <- [ \t] / '\n' / '\r\n' / '\r'
%

'numberlst'(I,A) -> 'numberlst'(I,A,0).
'numberlst'(I,A,0)->
  case '_sp'(I,A) of
    {f,Msg}->{f,Msg};
    {t,I2,A2}-> '([0-9]+ _sp)+'(I2,A2)
  end.

'([0-9]+ _sp)+'(I,A)->
  case '([0-9]+ _sp)'(I,A) of {t,I2,A2}->'([0-9]+_sp)*'(I2,A2);F->F end.

'([0-9]+ _sp)*'(I,A)->'([0-9]+ _sp)*'(I,A,[]).
'([0-9]+ _sp)*'(I,A,M)->
  case '([0-9]+ _sp)'(I,M) of
    {f,Msg}->{t,I,[lists:reverse(M)|A]};
    {t,I2,M2}->'([0-9]+ _sp)*'(I2,A,M2)
  end.

'([0-9]+ _sp)'(I,A)->
    case '[0-9]+'(I,A) of
      {f,Msg}->{f,Msg};
      {t,I2,A2}->'_sp'(I2,A2)
    end.

'_sp'(I,A)->'sp*',I,A).

%'sp*'(I,A)->'sp*'(I,A,[]).
%'sp*'(I,A,M)->
%  case 'sp'(I,M) of
%    {f,Msg}->{t,I,[lists:reverse(M)|A]};
%    {t,I2,M2}->'sp*'(I2,A,M2)
%  end.
'sp*'(<<C,R/binary>>,A,M) when C==$ ;C== $\t->'sp*'(R,A,[[C]|M]);
'sp*'(<<"\n",R/binary>>,A,M)->'sp*'(R,A,["\n"|M]);
'sp*'(<<"\r\n",R/binary>>,A,M)->'sp*'(R,A,["\r\n"|M]);
'sp*'(<<"\r",R/binary>>,A,M)->'sp*'(R,A,["\r\n"|M]);
'sp*'(I,A,M)->{t,I,[lists:reverse(M)|A]}.

'sp'(<<C,R/binary>>,A) when C==$ ;C== $\t->{t,R,[[C]|A]};
'sp'(<<"\n",R/binary>>,A)->{t,R,["\n"|A]};
'sp'(<<"\r\n",R/binary>>,A)->{t,R,["\r\n"|A]};
'sp'(<<"\r",R/binary>>,A)->{t,R,["\r"|A]};
'sp'(_,A)->{f,"err"}.

'[0-9]+'(I,A)->case '[0-9]'(I,A) of {t,I2,A2}->'[0-9]*'(I2,A2);F->F end.

'[0-9]*'(I,A)->'[0-9]*'(I,A,[]).
'[0-9]*'(<<C,R/binary>>,A,M) when C >= $0 and C =< $9->'[0-9]*'(R,A,[C|M]);
'[0-9]*'(I,A,M)->{t,I,[lists:reverse(M)|A]}.

'[0-9]'(<<C,R/binary>>, A) when C >= $0 and C =< $9->{t,R,[C|A]};
'[0-9]'(_,_)->{f,"err"}.



%parse(In) when is_list(In) -> parse(iolist_to_binary(In));
%parse(In) ->
%  case 'expr'(In, {1,1,1}, []) of
%    {true, _, AST} -> {ok, AST};
%    {false, Msg} -> {error, Msg}
%  end.

%'expr'(In, Pos, A) ->
%  case '_sp'(In, Pos, A) of
%    {false, Msg} -> {fail, Msg};
%    {true, Pos2, A2} ->
%      case '_sum'

'_sp'(In, Pos
