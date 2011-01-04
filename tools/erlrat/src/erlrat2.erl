-module(erlrat2).
-compile(export_all).

sample_peg_ast() ->
% >json_value <- s? (object/array/string/number/true/false/null) s?
% object      <- '{' s? pair (s? ',' s? pair)* s? '}'
%              / '{' s? '}' `object()`
% pair        <- s? string s? ':' s? json_value s? `pair()`
% >array      <- '[' s? json_value (s? ',' s? json_value)* s? ']'
%              / '[' s? ']' `array()`
% string      <- '"' (!'"' ("\\\\" / '\\"' / .))* '"' `string()`
% number      <- int frac? exp? `number()`;
% int         <- '-'? (nz_digit digit+) / digit
% frac        <- '.' digit+
% exp         <- e digit+
% e           <- [eE] ('+' / '-')?
% nz_digit    <- [1-9]
% digit       <- [0-9]
% true        <- 'true' `true`
% false       <- 'false' `false`
% null        <- 'null' `null`
% s           <- [ \t\n\s\r]*
