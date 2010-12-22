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
  {_ModuleName, ModuleAttrForm} = case find_module(ParsedForms) of
    {true, Nm} -> {Nm, []};
    false ->
      Nm = list_to_atom(filename:rootname(filename:basename(F))),
      {Nm,[{attribute,1,module,Nm}]}
  end,
  AllForms = lists:append([[{attribute, 1, file, {F, 1}}],
                           ModuleAttrForm,
                           ParsedForms,
                           [{eof,LastPos}]]),
  Res = compile:forms(AllForms, [binary,bin_opt_info,compressed,return]),
  case proplists:get_value(debug_info, Flags, false) of
    true -> info(F, ParsedForms, AllForms, Res);
    false ->
      % TODO: save the binary to the beam file
      % TODO: if the module name is a tuple, mkdir -p the correct output
      %       directories when saving.
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

find_module([]) -> false;
find_module([{attribute, _, module, Nm}|_]) -> {true, Nm};
find_module([_|T]) -> find_module(T).
