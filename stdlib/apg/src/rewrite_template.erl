-module(rewrite_template).
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
% <(|"hi there"|)> - Cool, matches string anywhere in there.
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
% --------------------------------------
%
% 0. Enumerate __ACHT_RW_ANON_M's (so they can be reused on the right)
% 1. Convert any non-agg lsigs into normal lists / nil-lists
%    - [] - nil
%    - [a b c] - normal list
%    - [a|R] (== [a(|R] ) - normal matching
%    - [a b c|R] - turn into [a|[b|[c|R]]]
% 2. If no remaining lsigs, write general rewrite rule
% 3. Any lsigs first match L when is_list(L) - ignores anything without the
%    possibility of a match so it can focus on the inner parts.
% 4. 
%
%
%
%  InTerm |= OutTerm
%    Single-scan == normal erlang match+return
%  If lsig with any aggregate clauses, we will need to iterate. Now question
%  is... how to iterate?
%
%  [a>       # 1⦅n⦆     == [a|_]
%  [a b c>   # 3⦅n⦆     == [a|[b|[c|_]]]
%            # Rule 1: if only one agg and it's on the far right, reducable to
%            # normal list, no iteration.
%
%  <a]       # ⦅n⦆1     == reverse list first, then [a|_] (then re-reverse after substitution)
%  <a b c]   # ⦅n⦆3     == r([c|[b|[a|_]]])
%            # Rule 2: if only one agg and it's on the far left, reducable to
%            # normal list with no iteration, but requires reversing logic.
%
%  <a>       # ⦅n⦆1⦅n⦆
%  <a b c>   # ⦅n⦆3⦅n⦆
%  <a..c>    # ⦅n⦆1⦅n⦆1⦅n⦆
%            # Rule 3a: Write the following one-pass simple-case catch:
%            #  |L| < sum(single-terms) -> no match;
%            # Rule 3b: Write one-pass simple-case match for when (non-specific) aggregates are empty
%            #  (try match as if no aggs at all in anon+Bind positions)  -> rewrite with empty aggs
%            # Rule 3c: 
%            #
%
%

