%%
%% TODO:
%%  - Error / warning tuples or output instead of mismatch, including on
%%    indents
%%  - Preprocessor:  includes, imports, syntax tranformations...
%%  - Postprocessor:
%%    - Consolidate/normalize parameters, packages, and module
%%      attributes- issue warnings / errors as necessary.
%%    - Map ungbar stdlib and insert function defs if necessary
%%  - Get package and parameter attributes to work
%%  - Auto-create ebin subdirectory for packages

-module(ungbar_compile).

-export([file/1,file/2,parse_forms/1]).

-define(DENT_IGN_BLOCKS, [
    % Type      | Start | End   | Skip  | Container? | Total Ignore?
    {atom,        "'",    "'",    true,   false,       false},
    {n_string,    "\"",   "\"",   true,   false,       false},
    {l_string,    "'''",  "'''",  true,   false,       false},
    {regex1,      "-/",   "/-",   true,   false,       false},
    %{regex2,      "r{",   "}",    true,   false,       false},
    {ml_comment,  "#|",   "|#",   true,   ["#|"],      true},
    {comment,     "#",    "\n",   false,  false,       true},
    {tuple,       "(",    ")",    false,  true,        false},
    {list,        "[",    "]",    ["|"],  true,        false},
    {clause,      "|",    "->",   ["|"],  true,        false},
    {binary,      "<[",   "]>",   false,  true,        false}
  ]).

%------------ File Compiling -------------------------------------------------
% Find option or default
opt([],outputdir)   -> ".";
opt([],bin_opt_info)-> true;
opt([],_)           -> [];
opt([{Key,V}|_],Key)-> V;
opt([_|T],Key)      -> opt(T,Key).

% Cull to only compiler options / defaults (more to come in the future)
get_compiler_options(Flags) ->
  Info = opt(Flags, info),
  {Info, lists:flatten(proplists:compact([
        case Info of true->return;_->report end,
        opt(Flags,bin_opt_info),
        opt(Flags,compressed),
        opt(Flags,debug_info),
        opt(Flags,verbose)
      ]))}.

file(F) -> file(F, []).
file(F, Flags) ->
  % Create indent tokens
  {ok, DentedBin} = indents:file_scan(F,
    [{ignoreblock_defs, ?DENT_IGN_BLOCKS}, {indent_token,6}, {dedent_token,21}]),
  case opt(Flags,show_indents) of true->debug_indents(DentedBin);_->nop end,

  {AST, ModuleName} = case parse_forms(DentedBin) of
    {ok, ParsedForms, LastPos} -> post_process_forms(ParsedForms, LastPos, F);
    Err -> error_logger:error_msg("Error: ~p", [Err]), halt(5)
  end,

  {Info, CompilerOpts} = get_compiler_options(Flags),
  Res = compile:forms(AST, CompilerOpts),

  case Info of true->debug_info(F,AST,Res);_->nop end,

  RealMName = case ModuleName of
    {MName, _Parameters} -> MName;
    MName -> MName
  end,
  OutFile = filename:join(opt(Flags,outputdir),[RealMName,".beam"]),
  case Res of
    {ok, _, Code}    -> file:write_file(OutFile, Code);
    {ok, _, Code, _} -> file:write_file(OutFile, Code);
    Err2 -> error_logger:error_msg("Error: ~p", [Err2]), halt(10)
  end.


%------------ Parsing Forms --------------------------------------------------

parse_forms(B) when is_binary(B) -> parse_forms(binary_to_list(B));
parse_forms(L) -> ungbar:parse(L).


%------------ Post Processor -------------------------------------------------

post_process_forms(AST, LastPos, F) ->
  % Look for module name, create one if necessary, and put it on top of the forms.
  {ModuleName, ModuleAttrForm, AST2} =
    case fixup_module(AST,[]) of
      {none, none, PF} ->
        Nm = list_to_atom(filename:rootname(filename:basename(F))),
        {Nm, {attribute,1,module,Nm}, PF};
      {Nm, MF, PF} -> {Nm, MF, PF}
    end,
  AST3 = lists:append([[{attribute, 1, file, {F, 1}}],
                           [ModuleAttrForm],
                           AST2,
                           [{eof,LastPos}]]),
  {AST3, ModuleName}.

fixup_module([],Acc) -> {none, none, lists:reverse(Acc)};
fixup_module([{attribute,_,module,Nm}=Mf|R],Acc) ->
  {Nm, Mf, lists:reverse(Acc) ++ R};
fixup_module([Attr|R],Acc) -> fixup_module(R,[Attr|Acc]).


%------------ Debug Printing -------------------------------------------------

debug_indents(DentedBin)->
    io:format(
      "+---------------------------------~n"
      "|  INDENTS & DEDENTS :~n"
      "+---------------------------------~n~s~n",
      [swapchr(21,"<<<",swapchr(6,">>>",binary_to_list(DentedBin)))]).

debug_info(Filename, AST, Compiled) ->
  io:format(
    "~n~n"
    "+---------------------------------~n"
    "|  PARSE RESULTS FOR ~s:~n"
    "+---------------------------------~n"
    "~P~n~n", [Filename, AST, 60]),
  io:format(
    "+---------------------------------~n"
    "|  ERLANG EQUIVALENT:~n"
    "+---------------------------------~n"
    "~s",
    [erl_prettypr:format(erl_syntax:form_list(AST))]),
  io:format(
    "+---------------------------------~n"
    "|  COMPILE RESULTS:~n"
    "+---------------------------------~n"
    "~P~n~n"
    "----------------------------------~n",
    [Compiled, 20]),

  io:format("(pausing for io)~n",[]),
  receive
    nothing -> ok
  after 400 -> ok
  end.

swapchr(C,R,L)      ->swapchr(C,R,L,[]).
swapchr(_,_,[],A)   ->lists:flatten(lists:reverse(A));
swapchr(C,R,[C|T],A)->swapchr(C,R,T,[R|A]);
swapchr(C,R,[H|T],A)->swapchr(C,R,T,[H|A]).
