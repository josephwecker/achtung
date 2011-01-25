-module(compiler_samples).
-compile(export_all).


% 1. Last expression in a seq shouldn't need to rewrite its succ/fail returns -
% just use its result directly.
% 2. Any expression in an xord shouldn't need its returns rewritten.
% 3. 
% 

% Original:   NL <- ([\r] special_nl // [\n]) NL / succ
'NL'({Bin,Idx,Line,Col,Misc,TmpAcc}) ->
  % - Token, so no accumulation - static result
  % - Was star expression - always succeeds
  % - Determinants - [\r] & [\n]
  % - ord1(seq1(xord1(seq2(char1(\r),
  %                        call1),
  %                   char3(\n)),
  %             recurse1),
  %       special1(succ))

  % case
  %   case
  %     case Bin of

  %      [\r] ->
  %        case 'special_nl-noacc'({NewPos...}) of
  %          {succ, S, _} -> {succ, (NewPos), []}; % Ign res and empty
  %          {fail, ...} -> {fail, ...}
  %        end

  % %%% BECOMES (due to rule #1)

  %      [\r] -> 'special_nl-noacc'({NewPos...})

  %      [\n] -> {succ, (NewPos), []} % Empty result because in Token
  %      _ -> {fail, ...}
  %     end
  %   of
  %    {fail, ...} -> {fail, ...};
  %    {succ, ...} -> recurse1   % Due to rule #1
  %   end
  % of
  %  {fail, ...} -> {succ, (NewPos), 'NL'};
  %  {succ, ...} -> {succ, (NewPos), 'NL'} % Can never occur-  detectable??? ok if not
  % end
  %
  %
  % 

% a

% Original:   S <-  (' ' / '\t' / '\n' / '\r')*
% Normalized: S <=- ([ ]//[\t]//[\n]//[\r]) S // succ
'S_input'() ->
  {'S',{xord,[{i,{1,8}},star,{orig,'S'},token],
      [{seq,[],
          [{xord,[],[{char,[{i,{1,8}}]," "},
                {char,[{i,{1,14}}],"\t"},
                {char,[{i,{1,21}}],"\n"},
                {char,[{i,{1,28}}],"\r"}]},
            {call,[],'S'}]},
        {special,[],succ}]}}.
% Determinants: [ ], [\t], [\n], [\r]

% determinants -> just recurse
% not determinants -> succeed w/ token

'S'({Bin,Idx,Line,Col,Misc,TmpAcc}, Final) ->
  % {succ, UpdatedState, Result} (updated index, position, and token result)
  % - Token
  % - Was * expression
  % - No tags

  case Bin of
    <<_:Idx/bytes, Next, _/bytes>>

    

