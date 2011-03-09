-module(rw_sample).
-compile(export_all).

%




%SEQ    ≔ (seq As Xs)
%PCH    ≔ (pch Ap Xp)
%XCH    ≔ (xch Ax Xx)
%LIT    ≔ (lit Al Xl)
%CHR    ≔ (chr Ac Xc)
%REF    ≔ (ref Ar Xr)
%EPS    ≔ (ɛ   Ae Xe)
%FAIL   ≔ (ϝ   Af Xf)
%
%E1     ≔ (ET1 Ae1 Xe1)
%E2     ≔ (ET2 Ae2 Xe2)
%E3     ≔ (ET3 Ae3 Xe3)
%
%⌀SEQ   ≔ (seq [] [])
%⌀PCH   ≔ (pch [] [])
%⌀XCH   ≔ (xch [] [])
%⌀LIT   ≔ (lit [] [])
%⌀CHR   ≔ (chr [] [])
%⌀REF   ≔ (ref [] [])
%⌀EPS   ≔ (ɛ   [] [])
%⌀FAIL  ≔ (ϝ   [] [])
%
%⌀E1    ≔ (ET1 [] [])
%⌀E2    ≔ (ET2 [] [])
%⌀E3    ≔ (ET3 [] [])
%general/simplification/remove-seq-nop
%  ## ɛ anywhere in a sequence doesn't do anything
%  ## A B ɛ C → A B C
%  | ((seq As Xs) <(\epsilon [] [])>|R]) ⊨ ((seq As Xs) <R])
%
%general/simplification/shortcircuit-dead-choice
%  ## Anything after an ɛ in an ord or xrd is unreachable
%  ## A/B/ɛ/C/D → A/B/ɛ
%  | (PCH <⌀EPS|R]) ⊨                          w"Unreachable code"; (PCH <⌀EPS])
%  | (XCH <⌀EPS|R]) ⊨                          w"Unreachable code"; (XCH <⌀EPS])
%
%general/simplification/remove-choice-nop
%  ## A term in an pch/xch that is guaranteed to fail can be skipped
%  ## A/B/ϝ/C/D → A/B/C/D
%  | (PCH <⌀FAIL|R]) ⊨                                                 (PCH <R])
%  | (XCH <⌀FAIL|R]) ⊨                                                 (XCH <R])
%
%attribute/simplification/opt-opt
%  | (E <opt opt> S) ⊨                                               (E <opt> S)



% Combination shortcuts
%general(Term) ->
%  apply_rules([fun 'general/simplification/r1/remove-dead-seq'/1,
%               fun 'general/simplification/remove-seq-nop'/1,
%               fun 'general/simplification/shortcircuit-dead-choice'/1,
%               fun 'general/simplification/remove-choice-nop'/1],Term).
%attribute(Term) ->
%  apply_rules([fun 'attribute/simplification/opt-opt'/1],Term).
%'general/simplification'(Term) ->
%  apply_rules([fun 'general/simplification/r1/remove-dead-seq'/1,
%               fun 'general/simplification/remove-seq-nop'/1,
%               fun 'general/simplification/shortcircuit-dead-choice'/1,
%               fun 'general/simplification/remove-choice-nop'/1],Term).
%'general/simplification/r1'(Term) ->
%  apply_rules([fun 'general/simplification/r1/remove-dead-seq'/1],Term).
%

%general/simplification/r1/remove-dead-seq
%  ## A ϝ term anywhere in a seq causes the whole thing to fail
%  ## A B ϝ D → ϝ
%  | ((seq As Xs) <('\digamma' [] [])>) ⊨ w"Sequence always fails"; ('\digamma' [] [])
%'general/simplification/r1/remove-dead-seq'({{seq,As,Xs},L}) when is_list(L) ->
%  case il_1(L) of

%'general/simplification/r1/remove-dead-seq'(Other) -> Other.


%------------------
%  a
%    | (o <opt..(opt N)>) |=  (p <N>)
%
%  # (expanded)
%
%  a |(o [⦅A1⦆opt⦅A2⦆(opt N)⦅A3⦆])|= (p [⦅A1⦆N⦅A3⦆])
%------------------

%a({o,L}=INTERM) when is_list(L) ->             % L<n> and is_list(L<n>) on every listsig
%                                               % MatchTerm=INTERM (to pass it
%                                               % along by itself if needed)
%  case il1(L,[],[]) of  % On each listsig
%    {A1,_,_,{opt,N},A3} -> {p,lists:append([A1,[N],A3])};   % Left side is
%                                                            % expanded version of listsig
%    _ -> INTERM
%  end;
%a(INTERM) -> INTERM.
%
%il1(L,_,_) when length(L)<2 -> nomatch;
%il1([opt|[{opt,N}|R]],A,_)  -> {lists:reverse(A),opt,[],{opt,N},R};
%il1([opt|R],A,_)            -> il2(R,[],[opt|[lists:reverse(A)]]);
%il1([O|R],A,_)              -> il1(R,[O|A],[]).
%
%il2([],_,_)                 -> nomatch;
%il2([{opt,N}|R],A,Res)      -> list_to_tuple(lists:reverse([R|[{opt,N}|[lists:reverse(A)|Res]]]));
%il2([O|R],A,Res)            -> il2(R,[O|A],Res).

[{'Gather_ARW_L1_ARW',
    % GROUP 1
  [{agg_term,{{line,1},{column,8}},{var,{{line,0},nil},'_ARW_L1'}},
   {match_term,{{line,1},{column,9}},opt},

    % GROUP 2
   {agg_term,{{line,1},{column,12}},{var,{{line,0},nil},'_ARW_M2'}},
   {match_term,{{line,1},{column,14}},
               {tuple,{{line,1},{column,14}},
                      [opt,{var,{{line,1},{column,19}},'N'}]}},

    % GROUP 3 / tailing agg
   {agg_term,{{line,1},{column,21}},{var,{{line,0},nil},'_ARW_R3'}}]}]


a({o, Gather_ARW_L1_ARW}=IN_TERM) when is_list(Gather_ARW_L1_ARW) ->
  case '--arw_lsig1--'(Gather_ARW_L1_ARW,[]) of
    nomatch -> IN_TERM;
    {_ARW_L1_ARW, opt, R1} ->
      case '--arw_lsig2--'(R1,[]) of
        nomatch -> IN_TERM;
        {_ARW_M2_ARW, {opt,N}, _ARW_R3_ARW} ->
          {p, lists:append([_ARW_L1_ARW, [N], _ARW_R3_ARW])}
      end
  end.

'--arw_lsig1--'([],A) -> nomatch;
'--arw_lsig1--'([opt|R],A) -> {lists:reverse(A),opt,R};
'--arw_lsig1--'([O|R],A) -> '--arw_lsig1--'(R,[R|A]).

% TODO: these inner ones act a lot like top-level ones- difference is the
% automatic accumulator and the return value for three different clauses.
% Inside, though, they need to be able to do lsig matching just like the
% top-level ones.

% TODO: right-side: listsigs turn into normal conses where possible, and only
% turn into lists:append when they have to

% TODO: the case-matching clauses for inner returns need to be fully expanded
% somehow so all inner variables get matched... might need to match the inner
% tuple, etc....

rw_name(GList1=In) when is_list(GList1) ->
  case lsig1(GList1,[]) of
    nomatch -> In;
    <normleft of 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%








a, m+, a, m+


%listsig(list,def) -> tuple



case listsig(list,def) of
  nomatch -> original-term
  {nested-listsig-tuple} -> rewrite-term
end



case '--arwlsig1--'(L_L1) of
  nomatch -> original-term;
  {full-nested-tuple-res} ->
    case '--arwlsig2--'(L_L2) of
      nomatch -> original-term;
      {full-nested-tuple-res2} ->
        right-side
    end
end

