-module(ungbarc).
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
  lists:foreach(fun(F)->ungbarc(Flags,F) end, Files).

parse_opts(Acc, ["-o", Outputdir | R]) ->
  parse_opts([{outputdir, Outputdir} | Acc], R);
parse_opts(Acc, ["-i" | R]) ->
  parse_opts([{debug_info, true} | Acc], R);
parse_opts(Acc, Files) ->
  {lists:reverse(Acc), Files}.


ungbarc(Flags, F) ->
  {ok, DentedBin} = indents:file_scan(F,[{ignoreblock_defs, ?DENT_IGN_BLOCKS},
                                         {indent_token,6},
                                         {dedent_token,21}]),
  % TODO: error formatting instead of mismatch
  {ok, ParsedForms, LastPos} = ungbar:parse(binary_to_list(DentedBin)),
  % TODO: insert module if not already present
  AllForms = lists:append([[{attribute, 1, file, {F, 1}}],
                           ParsedForms,
                           [{eof,LastPos}]]),
  Res = compile:forms(AllForms, [binary,bin_opt_info,compressed,return]),
  case proplists:get_value(debug_info, Flags, false) of
    true -> info(F, ParsedForms, AllForms, Res);
    false ->
      % TODO: save the binary to the beam file
      Res
  end.

info(Filename, ParsedForms, AllForms, Compiled) ->
  io:format("~n~n+---------------------------------~n"
            "|  PARSE RESULTS FOR ~s:~n"
            "+---------------------------------~n"
            "~p~n~n", [Filename, ParsedForms]),
  io:format("+---------------------------------~n"
            "|  ERLANG EQUIVALENT:~n"
            "+---------------------------------~n"
            "~s",
            [erl_prettypr:format(erl_syntax:form_list(AllForms))]),
  io:format("+---------------------------------~n"
            "|  COMPILE RESULTS:~n"
            "+---------------------------------~n"
            "~P~n~n----------------------------------~n",
            [Compiled, 20]),

  io:format("(pausing)~n",[]),
  receive
    nothing -> ok
  after 1000 -> ok
  end.
