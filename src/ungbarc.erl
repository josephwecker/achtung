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

% Not matching these up to erlc at the moment- likely will in the near future
parse_opts(A,["-o",Dir|R])   -> parse_opts([{outputdir,Dir}    |A],R);
parse_opts(A,["-i"|R])       -> parse_opts([{info,true}        |A],R);
parse_opts(A,["-v"|R])       -> parse_opts([{verbose,true}     |A],R);
parse_opts(A,["-d"|R])       -> parse_opts([{debug_info,true}  |A],R);
parse_opts(A,["--indents"|R])-> parse_opts([{show_indents,true}|A],R);
parse_opts(A,Files)          -> {lists:reverse(A), Files}.

% Defaults when default isn't nothing and it's not in parse_opts yet
opt([],outputdir)   -> ".";
opt([],bin_opt_info)-> true;

opt([],_)           -> [];
opt([{Key,V}|_],Key)-> V;
opt([_|T],Key)      -> opt(T,Key).


ungbarc(Flags, F) ->
  {ok, DentedBin} = indents:file_scan(F,[{ignoreblock_defs, ?DENT_IGN_BLOCKS},
                                         {indent_token,6},
                                         {dedent_token,21}]),
  case opt(Flags, show_indents) of
    true ->
      io:format("+---------------------------------~n"
                "|  INDENTS & DEDENTS :~n"
                "+---------------------------------~n~s~n",
                [swapchr(21,"<<<",swapchr(6,">>>",binary_to_list(DentedBin)))]);
    _ -> nothing
  end,
  % TODO: error formatting and output instead of mismatch
  {ok, ParsedForms, LastPos} = ungbar:parse(binary_to_list(DentedBin)),
  {ModuleName, ModuleAttrForm} = case find_module(ParsedForms) of
    {true, Nm} -> {Nm, []};
    false ->
      Nm = list_to_atom(filename:rootname(filename:basename(F))),
      {Nm,[{attribute,1,module,Nm}]}
  end,
  AllForms = lists:append([[{attribute, 1, file, {F, 1}}],
                           ModuleAttrForm,
                           ParsedForms,
                           [{eof,LastPos}]]),
  Info = opt(Flags, info),
  COpts = lists:flatten(proplists:compact([
        case Info of true->return;_->report end,
        opt(Flags,bin_opt_info),
        opt(Flags,compressed),
        opt(Flags,debug_info),
        opt(Flags,verbose)
      ])),
  Res = compile:forms(AllForms, COpts),
  case Info of
    true -> info(F, ParsedForms, AllForms, Res);
    [] -> nothing
  end,

  RealMName = case ModuleName of
    {MName, _Parameters} -> MName;
    MName -> MName
  end,
  % TODO: if the module name is a list, mkdir -p the correct output
  %       directories and correct the output path when saving.
  OutFile = filename:join(opt(Flags,outputdir),[RealMName,".beam"]),
  case Res of
    {ok, _, Code} -> file:write_file(OutFile, Code);
    {ok, _, Code, _} -> file:write_file(OutFile, Code);
    _ -> halt(10)
  end.



info(Filename, ParsedForms, AllForms, Compiled) ->
  io:format("~n~n+---------------------------------~n"
            "|  PARSE RESULTS FOR ~s:~n"
            "+---------------------------------~n"
            "~P~n~n", [Filename, ParsedForms, 60]),
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

  io:format("(pausing for io)~n",[]),
  receive
    nothing -> ok
  after 400 -> ok
  end.

find_module([]) -> false;
find_module([{attribute, _, module, Nm}|_]) -> {true, Nm};
find_module([_|T]) -> find_module(T).

swapchr(C,R,L)    ->swapchr(C,R,L,[]).
swapchr(_,_,[],A)   ->lists:flatten(lists:reverse(A));
swapchr(C,R,[C|T],A)->swapchr(C,R,T,[R|A]);
swapchr(C,R,[H|T],A)->swapchr(C,R,T,[H|A]).

