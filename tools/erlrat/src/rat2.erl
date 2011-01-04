-module(rat2).
-compile(export_all).



% Parser function:
% Params:   {Binary, Idx}, Position, State
% Return:   {succ, NewIdx, NewPos, Output, NewState} | {fail, {Pos, Reason}}
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
% (Terminals always expand at the next level ?)
%
% Always try to reduce multis (+/*) to a single accumulate/consume recursive
% function.
%   Example-
%     given: NL<-'\r\n'/'\r'/'\n'
%     given: COMMENT<-'#'(!NL .)* NL
%     The COMMENT rule becomes: COMMENT<-'#' (!('\r\n'/'\r'/'\n') .)* NL
%     The middle clause there is a multi- (In this case consumer because
%       COMMENT is a tokened rule and therefore won't use the result).
%     So the middle clause pseudo-code becomes:
%       clauses(!'\r\n','\r','\n') !eof consume+recurse)
%     Which
%       
%

%  (types of expression)
% rule -> existing rule
% ord  -> ordered choice
% seq  -> sequence
%
%  (literal terminals)
% lit  -> string
% char -> character class
% any  -> any
% 
%   (expression attributes)
% notp -> '!'
% andp -> '&'
% star -> '*'
% plus -> '+'
% opt  -> '?'
% tok  -> returns token (name in all uppercase)
% drop -> '{...}' dropped before parse transform
%
% ! and ? = no-op for grammar, but possibly used in parse-transform
% & and ? = no-op for grammar, but possibly used in parse-transform
% * and ? = *
% + and ? = *
% + = (one) and *
%
% GRAMMAR TRANSFORMS:
%  #. Separate into main real-functions (entry-points, recurse-points) &
%     lowerlevel atoms
%  #. Reduce all seq([],(seq,[])) to single sequence (automatic?)
%  #. Reduce all *?->*, +?->*, +->seq(1*)
%  #. Apply attributes (notp, andp, opt)
%  #. Separate out cons/agg functions
%
% Need to be their own functions: cons/agg, entry-points, recurse-points
% All else can be turned into a single function
%
% test(Input,ToMatch,Match,NoMatch)
%   when ToMatch is lit == case clause
%   when ToMatch is rule== last-result->case-expression
%
% test(Inp,Pos,Match,NoMatch)
%
% NL<-'\r\n'/'\r'/'\n'
%
% m('\r\n',consume_succ,
%   m('\r',consume_succ,
%     m('\n',consume_succ,fail)))
%
% !NL
% m('\r\n',fail,
%   m('\r',fail,
%     m('\n',fail, consume_succ)))
%
% COMMENT
% 
%
%
% Arith:
%   !ord(a,b,c) = seq(!a,!b,!c)  (because predicates behave abnormally- no consuming/accumulating)
%   !ord(a,b,c) = inverse_ord(a,b,c)
% 
% create_[tok]_[ord|seq|one]_[z][
% consume is implied when 'tok' and 'z' or 'p'


% ------------------- Grammar line ----------------------
% NL "newline" <- '\r\n' / '\r' / '\n'
%
% ------------------- PEG AST ---------------------------
% NL/newline (ord|_|[(lit|_|"\r\n"),
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
    <<_:Idx/bytes,C:1/bytes,_/bytes>> -> {fail, {Pos, {unexpected, C, "end of file"}}};
    _ -> {succ, Idx, Pos, ':EOF', State}
  end.
      
'any'({Bin,Idx}=Input,{Line,Col}=Pos,State) ->
  case Bin of
    <<_:Idx/bytes,$\r,$\n,_/bytes>> -> {succ, Idx+2, {Line+1,0}, $\n, State};
    <<_:Idx/bytes,$\r,_/bytes>> -> {succ, Idx+1, {Line+1,0}, $\n, State};
    <<_:Idx/bytes,$\n,_/bytes>> -> {succ, Idx+1, {Line+1,0}, $\n, State};
    <<_:Idx/bytes,C:1/bytes,_/bytes>> -> {succ, Idx+1, {Line,Col+1}, C, State};
    _ -> {fail, {Pos, {unexpected, eof, "anything"}}}
  end.

'NL'({Bin,Idx},{Line,Col}=Pos,State) ->
  case Bin of
    <<_:Idx/bytes,$\r,$\n,_/bytes>> -> {succ, Idx+2, {Line+1,0}, ':NL', State};
    <<_:Idx/bytes,$\r,_/bytes>> -> {succ, Idx+1, {Line+1,0}, ':NL', State};
    <<_:Idx/bytes,$\n,_/bytes>> -> {succ, Idx+1, {Line+1,0}, ':NL', State};
    <<_:Idx/bytes,C:1/bytes,_/bytes>> -> {fail, {Pos, {unexpected, C, "newline"}}};
    _ -> {fail, {Pos, {unexpected, eof, "newline"}}}
  end.

'SPACE'({Bin,Idx},{Line,Col}=Pos,State) ->
  case Bin of
    <<_:Idx/bytes,$ ,_/bytes>> -> {succ, Idx+1, {Line,Col+1}, ':SPACE', State};
    <<_:Idx/bytes,$\t,_/bytes>>-> {succ, Idx+1, {Line,Col+1}, ':SPACE', State};
    <<_:Idx/bytes,C:1/bytes,_/bytes>> -> {fail, {Pos, {unexpected, C, "space"}}};
    _ -> {fail, {Pos, {unexpected, eof, "space"}}}
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
'COMMENT'({Bin,Idx},{Line,Col}=Pos,State) ->
  case Bin of
    <<_:Idx/bytes,$#,_/bytes>> ->
      {succ, Idx2, Pos2, none, State2} = 'COMMENT:2'({Bin,Idx+1},{Line,Col+1},State), % always succ
      case 'NL'({Bin,Idx2},Pos2,State2) of
        {succ, Idx3, Pos3, _, State3} -> {succ, Idx3, Pos3, ':COMMENT', State3};
        Fail -> Fail
      end;
    <<_:Idx/bytes,C:1/bytes,_/bytes>> -> {fail, {Pos, {unexpected, C,{"comment", "#"}}}};
    _ -> {fail, {Pos, {unexpected, eof, {"comment", "#"}}}}
  end.

'COMMENT:2'({Bin,Idx}=Inp,{Line,Col}=Pos,State) ->
  case 'NL'({Bin,Idx},Pos,State) of % succ/fail are inverted
    {succ,_,_,_,_} -> {succ, Idx, Pos, none, State};
    {fail,_} ->
      case 'any'(Inp,Pos,State) of
        {succ, Idx2, Pos2,_,State2} -> 'COMMENT:2'({Bin,Idx2},Pos2,State2);
        {fail,_} -> {succ, Idx, Pos, none, State}
      end
  end.

% ------------------- Grammar line ----------------------
% EOL        <- NL / COMMENT    E`Expecting a newline here`
%
% ------------------- PEG AST ---------------------------
% EOL (ord|_|[(rul|_|COMMENT)
%             (rul|_|NL)])
%
% ------------------- Generator function call -----------
% create_tok
%
% ------------------- Final output ----------------------
'EOL'({Bin,Idx}=Inp,{Line,Col}=Pos,State) ->
  case 'NL'(Inp,Pos,State) of
    {succ, Idx2, Pos2, _, State2} -> {succ, Idx2, Pos2, ':EOL', State2};
    {fail, Fail1} ->
      case 'COMMENT'(Inp,Pos,State) of
        {succ, Idx2, Pos2, _, State2} -> {succ, Idx2, Pos2, ':EOL', State2};
        {fail, Fail2} -> {fail, [Fail1,Fail2]}
      end
  end.

% ------------------- Grammar line ----------------------
% _S         <- (SPACE / EOL)*
%
% ------------------- PEG AST ---------------------------
% _S (ord|*|[(rul|_|SPACE)
%            (rul|_|EOL)])
%
% ------------------- Generator function call -----------
% create_tok_
%
% ------------------- Final output ----------------------

