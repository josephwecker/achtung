-module(rat2).
-compile(export_all).



% Parser function:
% Params:   {Binary, Idx}, Position, State
% Return:   {succ, NewIdx, NewPos, Output, NewState} | {fail, Pos, Reason}
%   Reason -> {unexpected, FoundChar, Expecting}
%           | {...
%
%   Expecting -> label_string | {rule_string, char(s)}

% Parser function creator
% Params:   (depends on type), TransformationFun, ErrorFun(?), StateTransFun(?)
% Return:   specialized parser function

% Grammar file AST :: [Rules]
% Rule             :: {Name, Parts, Transform}
% Name             :: RuleName
%                   | {RuleName, Label}
%
% Parts            :: 


% Common patterns:
% Rule's expression is a sequence of one, then it is an alias
% [not, not, ...] any
%
% "funable" -> terminal expression that can be expressed in a single erlang function
%
% Terminals always expand at the next level
%
%
%



% ------------------- Grammar line ----------------------
% NL "newline" <- '\r\n' / '\r' / '\n'
%
% ------------------- PEG AST ---------------------------
% NL/newline (pri|_|[(lit|_|"\r\n"),
%                    (lit|_|"\r"),
%                    (lit|_|"\n")])
% 
% ------------------- Generator function call -----------
% create_tok_p_choice({'NL',"newline"}, ["\r\n", $\r, $\n])
%
% ------------------- Final output ----------------------

%%% BUILTIN! (and hopefully inlined)
'EOF'({Bin,Idx},Pos,State) ->
  case Bin of
    <<_:Idx/bytes>> -> {succ, Idx, Pos,':EOF', State};
    <<_:Idx/bytes,C:1/bytes,_/bytes>> -> {fail, Pos, {unexpected, C, "end of file"}, State};
    _ -> {succ, Idx, Pos, ':EOF', State}
  end.
      
'any'({Bin,Idx}=Input,{Line,Col}=Pos,State) ->
  case Bin of
    <<_:Idx/bytes,$\r,$\n,_/bytes>> -> {succ, Idx+2, {Line+1,0}, $\n, State};
    <<_:Idx/bytes,$\r,_/bytes>> -> {succ, Idx+1, {Line+1,0}, $\n, State};
    <<_:Idx/bytes,$\n,_/bytes>> -> {succ, Idx+1, {Line+1,0}, $\n, State};
    <<_:Idx/bytes,C:1/bytes,_/bytes>> -> {succ, Idx+1, {Line,Col+1}, C, State};
    _ -> {fail, Pos, {unexpected, eof, "anything"}, State}
  end.

'NL'({Bin,Idx},{Line,Col}=Pos,State) ->
  case Bin of
    <<_:Idx/bytes,$\r,$\n,_/bytes>> -> {succ, Idx+2, {Line+1,0}, ':NL', State};
    <<_:Idx/bytes,$\r,_/bytes>> -> {succ, Idx+1, {Line+1,0}, ':NL', State};
    <<_:Idx/bytes,$\n,_/bytes>> -> {succ, Idx+1, {Line+1,0}, ':NL', State};
    <<_:Idx/bytes,C:1/bytes,_/bytes>> -> {fail, Pos, {unexpected, C, "newline"}, State};
    _ -> {fail, Pos, {unexpected, eof, "newline"}, State}
  end.

'SPACE'({Bin,Idx},{Line,Col}=Pos,State) ->
  case Bin of
    <<_:Idx/bytes,$ ,_/bytes>> -> {succ, Idx+1, {Line,Col+1}, ':SPACE', State};
    <<_:Idx/bytes,$\t,_/bytes>>-> {succ, Idx+1, {Line,Col+1}, ':SPACE', State};
    <<_:Idx/bytes,C:1/bytes,_/bytes>> -> {fail, Pos, {unexpected, C, "space"}, State};
    _ -> {fail, Pos, {unexpected, eof, "space"}, State}
  end.



% ------------------- Grammar line ----------------------
% COMMENT    <- '#' (!NL .)* NL
%
% ------------------- PEG AST ---------------------------
% COMMENT  (seq|_|[(lit|_|"#")
%                  (seq|*|[(rul|!|NL)
%                          (lit|.|_)])
%                  (rul|_|NL)])
%
% ------------------- Generator function call -----------
% create_tok_p_seq('COMMENT', [$#, clauses_consume_zero_or_more([{not, 'NL'}, any]), 'NL'])
%
% ------------------- Final output ----------------------
'COMMENT:1'({Bin,Idx},{Line,Col}=Pos) ->
  case Bin of
    <<_:Idx/bytes,$#,_/bytes>> ->
      {succ, Idx2, Pos2, none} = 'COMMENT:2'({Bin,Idx+1},{Line,Col+1}),  % Simplified because zero_or_more always succeeds
      'COMMENT:3'({Bin,Idx2},Pos2);  % Simplified because last in the sequence- propagate result directly
    <<_:Idx/bytes,C:1/bytes,_/bytes>> -> {fail, {unexpected, C, Pos, {"comment", "#"}}};
    _ -> {fail, {unexpected, eof, Pos, {"comment", "#"}}}
  end.
'COMMENT:2'({Bin,Idx},{Line,Col}=Pos,S) ->
  case 'NL'({Bin,Idx},Pos) of
    {succ, Idx2, Pos2, _} -> {succ, Idx, Pos, none};
    {fail, _} ->
