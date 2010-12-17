-module(cerlish).
-export([main/1]).

-define(DENT_IGN_BLOCKS, [
    % Type      | Start | End   | Esc?  | Container? | Total Ignore?
    {atom,        "'",    "'",    true,   false,       false},
    {n_string,    "\"",   "\"",   true,   false,       false},
    {l_string,    "'''",  "'''",  true,   false,       false},
    {regex1,      "/",    "/",    true,   false,       false},
    {regex2,      "r{",   "}",    true,   false,       false},
    {ml_comment,  "#|",   "|#",   true,   false,       true},
    {comment,     "#",    "\n",   false,  false,       true},
    {tuple,       "(",    ")",    false,  true,        false},
    {list,        "[",    "]",    false,  true,        false},
    {binary,      "<[",   "]>",   false,  true,        false},
    {line_cont,   "\\n",  "\n",   true,   true,        true}]).

main(Opts) ->
  {Flags, Files} = parse_opts([], Opts),
  lists:foreach(fun(F)->cerlish(Flags,F) end, Files).

parse_opts(Acc, ["-o", Outputdir | R]) ->
  parse_opts([{outputdir, Outputdir} | Acc], R);
parse_opts(Acc, Files) ->
  {lists:reverse(Acc), Files}.

cerlish(_Flags, F) ->
  {ok, DentedBin} = indents:file_scan(F,
    [{ignoreblock_defs, ?DENT_IGN_BLOCKS}, {indent_token, 6}, {dedent_token, 21}]),
  AST = erlish:parse(binary_to_list(DentedBin)),
  %AST = [{attribute,1,file,{F,1}} |
  %  erlish:parse(binary_to_list(DentedBin))],
  %io:format("~n~p~n", [compile:forms(AST)]),
  io:format("~nPARSE RESULTS FOR ~s:~n~p~n", [F, AST]),
  io:format("~n(pausing)~n",[]),
  receive
    nothing -> ok
  after 1000 -> ok
  end.
