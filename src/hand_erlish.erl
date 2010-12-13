-module(hand_erlish).

-compile(export_all).



parse(Input) ->
  'grammar'(Input, {{line, 1},{column,1}}).

'grammar'(Input, Idx) ->
  ['ws?'(Input), 'tl_exprs?'(Input), 'ws?'(Input)].

%'tl_exprs'(Input, Idx) ->

'comment'(<<"#",R/binary>>) ->
  {ok, I2, R} = '!nl*'(R),
  case 'nl'(Input2) of
    fail -> fail;
    {ok, Input3, Tr} -> {ok, Input3, []}
  end;
'comment'(_) -> fail.

'!nl*'(

% --> Sequence


% --> Options
'nl'(<<"\r\n",R/binary>>) -> {ok, R, []};
'nl'(<<"\n",R/binary>>) -> {ok, R, []};
'nl'(<<"\r",R/binary>>) -> {ok, R, []};
'nl'(_) -> fail.

'sp'(Input) ->
  case 'space+'(Input) of
    fail -> fail;
    Tr -> []
  end.

'space+'(Input) ->
  case 'space'(Input) of
    {ok, Input2, Tr} -> 'space+'(Input2, [Tr]);
    fail -> fail
  end.
'space+'(Input, Acc) ->
  case 'space'(Input) of
    {ok, Input2, Tr} -> 'space+'(Input2, [Tr | Acc]);
    fail -> {ok, Input, lists:reverse(Acc)}
  end.


'space'(<<C,R/binary>>) when C =:= 32 or C =:= 9 ->
  {ok, R, []};
'space'(_) ->
  fail;



% --> Options
'rule'(<<"\r\n",R/binary>>) -> {ok, R, []};
'rule'(<<"\n",R/binary>>) -> {ok, R, []};
'rule'(<<"\r",R/binary>>) -> {ok, R, []};
'rule'(_) -> fail.

