-module(dummy).
%-module(tmp.dummy, [Name]).
%-export([myfun/1]).
%-import(other_module, [other_fun/1]).
%-compile({inline, [myfun/1]}).
%-vsn("0.3.1").
%-author("Awesome").
%-include("include/ungbar.hrl").
%-include_lib("xmerl/include/xmerl.hrl").
-export([f/0]).
f() ->
  fun f/0,
  fun lists:reverse/1,
  %fun dude.dummy:f/0,
  fun() -> a end.
  %lists:reverse/1.

%-module(test.dummy, [Name]).
%-export([my_function/2]).
%-export([my_function/0, my_function/1]).
%-record(some_record, {f1, f2, f3}).
%-define(THIS, this).
%-define(THAT(Thing), [that, Thing]).

%-type my_type() :: atom() | integer().
%-spec my_function(integer()) -> integer().

%my_function() -> hello.
%my_function([Inside]) -> io:format("~p", [Inside]);
%my_function(_) -> io:format("hmmm", []).

%another(A,B,C,{D,E}) when is_binary(B) -> [D,E,B,C,A].

%my_function([_, this_pattern, V] = H, [_, other_pattern, W]) when V > W -> ok.
%my_function(_,_) ->
%  12390,
%  $c,
%  'asdf%f4289##',
%  "Hi \x{2504}",
%  ok.
