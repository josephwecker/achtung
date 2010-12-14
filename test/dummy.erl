-module(test.dummy, [Name]).
-export([my_function/2]).
%-export([my_function/0, my_function/1]).
%-import(other_module, [other_fun/1]).
%-compile({inline, [{my_function,0}, my_function/1]}).
%-record(some_record, {f1, f2, f3}).
%-vsn("0.3.1").
%-author("Awesome").
%-include("Something.hrl").
%-define(THIS, this).
%-define(THAT(Thing), [that, Thing]).

%-type my_type() :: atom() | integer().
%-spec my_function(integer()) -> integer().

%my_function() -> hello.
%my_function([Inside]) -> io:format("~p", [Inside]);
%my_function(_) -> io:format("hmmm", []).

%another(A,B,C,{D,E}) when is_binary(B) -> [D,E,B,C,A].

my_function([_, this_pattern, V] = H, [_, other_pattern, W]) when V > W -> ok.
