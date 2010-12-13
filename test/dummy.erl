-module(test.dummy, [Name]).

-export([my_function/0, my_function/1]).

my_function() -> hello.
my_function([Inside]) -> io:format("~p", [Inside]).
