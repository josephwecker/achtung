-module(ungbarc).
-export([main/1]).

main(Opts) ->
  {Flags, Files} = parse_opts([], Opts),
  lists:foreach(fun(F)->ungbar_compile:file(F, Flags) end, Files).

% Not matching these up to erlc at the moment- likely will in the near future
parse_opts(A,["-o",Dir|R])   -> parse_opts([{outputdir,Dir}    |A],R);
parse_opts(A,["-i"|R])       -> parse_opts([{info,true}        |A],R);
parse_opts(A,["-v"|R])       -> parse_opts([{verbose,true}     |A],R);
parse_opts(A,["-d"|R])       -> parse_opts([{debug_info,true}  |A],R);
parse_opts(A,["--indents"|R])-> parse_opts([{show_indents,true}|A],R);
parse_opts(A,Files)          -> {lists:reverse(A), Files}.

