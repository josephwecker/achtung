-module(rw_sample).
-export(compile_all).

%
% ⦅⦆⦇⦈
% --------------------------------------
% []           - Matches an empty list
%                Example: [] = []
% [A B]        - Matches list with A and B exactly
%                Example: [A b] = [1 b]
% [H|T]==[H⦅T] - Head is one element and tail has the rest as a list
%                Example: [a|[b c d]] = [a ⦅b c d] = [a ⦅b ⦅c d] = [a b c d]
% [H⦆T]        - Head matches the whole list except the last element, matched by T
%                Example: [a b c⦆ d] = [a b c d] ; [K⦆ c d] = [a b c d] #| K == [a b] |#
% [H⦅M⦆T]      - H for first element, T for last, and M for all in between
%                Example; [a ⦅b c⦆ d] = [a b c d] ;  [A ⦅X⦆ b c d] = [a b c d] #| A==a, X==[] |#
% [A⦅B⦆⦅C⦆D]   - Future? Rewrite matching only so it can trigger multiple
%                times? Only if both aren't generic unassigned matchers? ??
% [H⦆M⦅T]      - Just like <M> but captures/matches lists on either side of it as well.
%                Example: [L⦆wow⦅R] = [and wow are you cool] #| L==[and], R==[are you cool] |#
% <M⦅T]==<M|T] - Equivalent also to [_⦆M⦅T] - So finds M with anything
%                (unmatched / uncaptured) before it, and captures the T after it.
% <A…B>        - Matches anywhere that B comes after A. (Pretty slow if it's a
%                big list and both variables aren't bound, of course...)
% [A a B>      - Matches the three at the beginning with anything afterward.
%
% [A a B⦆ ⦅_]  - Same as last one, but in a kind of obtuse way...
%
% --------------------------------------
%
% ## LEXICAL EQUIVALENTS / SYMBOLS
%  ⦅    ==  (|     ~= |      # Equivalent except the pipe requires the list brackets afterward for inner stuff.
%  ⦅_⦆  ==  (|_|)  == ..     == …
% [_⦆   == [_|)    == [(|_|) == <
%   ⦅_] ==   (|_]  == (|_|)] == >
%
% ## e.g.,
% [(|_|) T (|_|) U (|R|)] == [_⦆T⦅_⦆U⦅R] == <T..U|R] == <T…U|R]
%
% --------------------------------------
%
% lsig, match_term, agg_term
%


% Return 
%
%
% listsig types
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
%general/simplification/r1/remove-dead-seq
%  ## A ϝ term anywhere in a seq causes the whole thing to fail
%  ## A B ϝ D → ϝ
%  | ((seq As Xs) <(\digamma [] [])>) ⊨ w"Sequence always fails"; (\digamma [] [])
%
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
general(Term) ->
  apply_rules([fun 'general/simplification/r1/remove-dead-seq'/1,
               fun 'general/simplification/remove-seq-nop'/1,
               fun 'general/simplification/shortcircuit-dead-choice'/1,
               fun 'general/simplification/remove-choice-nop'/1],Term).
attribute(Term) ->
  apply_rules([fun 'attribute/simplification/opt-opt'/1],Term).
'general/simplification'(Term) ->
  apply_rules([fun 'general/simplification/r1/remove-dead-seq'/1,
               fun 'general/simplification/remove-seq-nop'/1,
               fun 'general/simplification/shortcircuit-dead-choice'/1,
               fun 'general/simplification/remove-choice-nop'/1],Term).
'general/simplification/r1'(Term) ->
  apply_rules([fun 'general/simplification/r1/remove-dead-seq'/1],Term).


'general/simplification/r1/remove-dead-seq'({{seq,As,Xs},L}) when is_list(L) ->
  case il_1(L) of

'general/simplification/r1/remove-dead-seq'(Other) -> Other.

