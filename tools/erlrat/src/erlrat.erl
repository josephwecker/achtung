-module(erlrat).
% Clean up a bit
% Optimize consumption
% Binary based (also only pass index around to backtrack on etc.)
% Figure out easy way to add errors / warnings
% Better syntax for grammar itself, including:
%   - ignore via leading underscore for much easier transforming
%   - fix annoying issue where space after closing parenthases is required?
%   - cleaner end-of-rule syntax
%   - lots of built-in utilities for extraction - generally just one function call:
%     some_rule <- '[' _s expression (_s ',' _s expression)* ']' -> make_list # ( make_list([expression, [expression, ...]]) )
%   - strings with single-quotes are like rules with underscores- auto-ignored
% Figure out a better way to memoize somehow- faster & less memory...
%   - at the very least, a named ets-table so it doesn't have to be fetched from the dictionary
%   - possibly expire older tuples via query when we know we won't need them anymore
%   - maybe only store a bit value w/ succeed/fail and basically build a final
%     parse function that knows exactly what it's looking for based off of those flags, which
%     then really consumes the string and does all the parse-transforming in a second "pass"-
%     Final pass would resemble the "success" chain of calls in the current single-pass.
%     I'll bet if it were visualized it would look an awful lot like a maze-solving algorithm.
% 

% p_optional
% p_assert_not
% p_assert
% p_seq
% p_choose
% p_zero_or_more
% p_one_or_more
% p_string
% p_anything
% p_char_class

% *** Which (when applied to char-class, any, or a string) can be treated as composable function clauses?


% lines <- line*
% line  <- (!nl .)* _nl
% nl "Newline"   <- "\r\n" / "\n" / "\r" / !.


% Built-in rules


%%%  some_rule <- rule2 rule3 (rule4 / "blah" / [0-9_])
%%%  rule4  <- ":"?
%%%
%%%  some_rule <- rule2 rule3 (rule4 / `generate_consume_1(["blah",[$0,$9],$_])`)
%%%  rule4  <- ":"?
%%%
%%%  some_rule <- rule2 rule3 (rule4 / `generate_consume_1(["blah",[$0,$9],$_])`)
%%%  rule4  <- `maybe-consume(":")`
%%%
%%%  some_rule <- rule2 rule3 (`maybe_consume(":")`++`generate_consume_1(["blah",[$0,$9],$_])`)
%%%                            # ^^^ Single four-clause function

%%%
%%%  some_rule <- rule3 rule4 ("blah" / [0-9] / "doink")+
%%%  rule4  <- `maybe-consume(":")`
%%%
%%%  some_rule <- rule3 rule4 (`generage_consume_plus(["blah", [$0,$9], "doink"])
%%%  rule4  <- `maybe-consume(":")`
%%%
%%%  some_rule <- rule3 `maybe-consume(":")` (`generage_consume_plus(["blah", [$0,$9], "doink"])
%%%  rule4  <- `maybe-consume(":")`
%%%
% generate_consumer


% ?gen_p_choose('newline', "Newline", ["\r\n", "\n", "\r"])
'newline'(<<"\r\n",R/binary>>,{L,_C}) ->  {success, "\r\n", R, {L+1,0}};  % MATCH MULTI
'newline'(<<$\n,  R/binary>>, {L,_C}) ->  {success, $\n,    R, {L+1,0}}; % MATCH CHAR
'newline'(<<$\r,  R/binary>>, {L,_C}) ->  {success, $\r,    R, {L+1,0}};
'newline'(_, Pos) -> {fail, {expected, "Newline", Pos}}.

% ?gen_p_char_class("[A-Z_]") -->  ?gen_p_choose('[A-Z_]', "'A-Z' or '_'", 
% [A-Z_]
'[A-Z_]'(<<N, R/binary>>, {L,C}) when (N >= $A and N =< $Z) -> {success, N, {L,C+1}}; % MATCH RANGE
'[A-Z_]'(<<$_,R/binary>>, {L,C}) -> {success, $_, {L, C+1}};
'[A-Z_]'(_, Pos) -> {fail, {expected, "'A-Z' or '_'", Pos}}.


