%% Run indent/detent algorithm and inject indent and dedent tokens (9 and 21)
%% when appropriate.  Tabs are treated as 2 spaces.  An EOF (<<4>>) causes it
%% to put on any remaining dedents it needs to.
%%
%% file_scan(Filename) gives the result of scanning the file as a binary.  If
%%    there are any errors it gives {LineNumber, Error} instead.
%% full_scan(Data) automatically appends an EOF so it can be used for strings
%%    etc. that you know are "complete"
%% scan(Data) or scan(Data, PreviousState) allow you to get results back in a
%%    streaming environment / continuous way. Gives back either
%%    {ok, ResultSoFar, CurrentState} or an error.
%%
%% == Implementation ==
%% Algorithm (basically) - to quote documentation from Python:
%% Before the first line of the file is read, a single zero is pushed on the
%% stack; this will never be popped off again. The numbers pushed on the stack
%% will always be strictly increasing from bottom to top. At the beginning of
%% each logical line, the line’s indentation level is compared to the top of the
%% stack. If it is equal, nothing happens. If it is larger, it is pushed on the
%% stack, and one INDENT token is generated. If it is smaller, it must be one of
%% the numbers occurring on the stack; all numbers on the stack that are larger
%% are popped off, and for each number popped off a DEDENT token is generated. At
%% the end of the file, a DEDENT token is generated for each number remaining on
%% the stack that is larger than zero.
%%
%% The State tuple that is passed around is this:
%%   {Aumulator, IndentStack, CurrentIndentLevel, CurrentState} where
%%     CurrentState is one of 'start' or 'inline'
%%
%% DOUBLE+ INDENT (determined by first valid indent) gets exempted
%%


-module(indents).
-export([file_scan_test/0, file_scan/1, full_scan/1, scan/1, scan/2]).
-define(INIT, {<<>>,[0],0,start}).

-define(DEFAULT_IGN_BLOCKS, [
    % Type    | Start | End   | Esc?  | Container?
    {string,   "\"",   "\"",   true,   false},
    {comment,  "#",    "\n",   false,  false},
    {args,     "(",    ")",    false,  true},
    {selector, "[",    "]",    false,  true}]).

file_scan_test() ->
  fprof:trace(start),
  file_scan("test/indents-test-text.txt"),
  file_scan("test/indents-test-text.txt"),
  file_scan("test/indents-test-text.txt"),
  file_scan("test/indents-test-text.txt"),
  file_scan("test/indents-test-text.txt"),
  file_scan("test/indents-test-text.txt"),
  fprof:trace(stop),
  fprof:profile(),
  fprof:analyse([{dest, "indents.analysis.erl"}, {cols, 50}, no_callers, {totals, true}]).

file_scan(FName)->
  % We're going to be nice and not pull it all into memory to begin with.
  {ok, File} = file:open(FName, [raw, binary, {read_ahead, 512}]),
  do_file_scan(File).

full_scan(Data)->dents(erlang:iolist_to_binary([Data,4]),?INIT).

scan(Data) -> dents(Data, ?INIT).
scan(Data, {_,_,_, inline} = State) -> next(Data, State);
scan(Data, State) when is_list(Data) -> dents(erlang:iolist_to_binary(Data), State);
scan(Data, State) -> dents(Data, State).

do_file_scan(File) ->
  do_file_scan(File, 1, ?INIT).
do_file_scan(File, LineNum, State) ->
  case file:read_line(File) of
    eof -> case dents(<<4>>, State) of
        {ok, {Fin, _, _, _}} -> {ok, Fin};
        Other -> {LineNum, Other}
      end;
    {ok, Data} -> case dents(Data, State) of
        {ok, State2} -> do_file_scan(File, LineNum+1, State2);
        Other -> {LineNum, Other}
      end;
    Other -> {LineNum, Other}
  end.

% Indents
dents(<<32, R/binary>>, {A, IS, CI, _}) -> dents(R, {<<A/binary,32>>, IS, CI+1, start});
dents(<<9, R/binary>>, {A, IS, CI, _}) -> dents(R, {<<A/binary,9>>, IS, CI+2, start});

% Essentially blank lines- throw away current indent level & continue
dents(<<"\r\n",R/binary>>,{A,IS,_,_})->dents(R,{<<A/binary,$\r,$\n>>,IS,0,start});
dents(<<$\n,R/binary>>,{A,IS,_,_})->dents(R,{<<A/binary,$\n>>,IS,0,start});
dents(<<$\r,R/binary>>,{A,IS,_,_})->dents(R,{<<A/binary,$\r>>,IS,0,start});
%dents(<<$#,R/binary>>,{A,IS,_,_})->next(R,{<<A/binary,$#>>,IS,0,inline});
dents(<<4>>,{A,IS,CI,CS})->
  case dedents(A,IS,0) of {A2,_}->{ok,{A2,IS,CI,CS}};E->E end;

% Same indent level as last.  Moving along...
dents(<<C,R/binary>>,{A,[L|_]=IS,L,_})->next(R,{<<A/binary,C>>,IS,0,inline});
% Indent
dents(<<C,R/binary>>,{A,[L|_]=IS,CI,_}) when CI>L->next(R,{<<A/binary,6,C>>,[CI|IS],0,inline});
% Dedent and/or propagate error
dents(<<C,R/binary>>,{A,IS,CI,_})->
  case dedents(A, IS, CI) of {A2,IS2}->next(R,{<<A2/binary,C>>,IS2,0,inline});E->E end;
% Done (... for now, muahaha)
dents(<<>>,{A,IS,CI,_})->{ok,{A,IS,CI,start}}.

% Keep dedenting & popping until we're lined up again
dedents(A,[],CI)->{indent_error,A,empty,CI};
dedents(A,[L|R],L)->{A,[L|R]};
dedents(A,[L|_R],CI)when CI>L->{indent_error,A,L,CI};
dedents(A,[_L|R],CI)->dedents(<<A/binary,21>>,R,CI).

%% Skip ahead to right after the next newline
next(<<>>,{A,IS,_,_})->{ok,{A,IS,0,inline}};
next(<<"\r\n",R/binary>>,{A,IS,_,_})->dents(R,{<<A/binary,$\r,$\n>>,IS,0,start});
next(<<$\n,R/binary>>,{A,IS,_,_})->dents(R,{<<A/binary,$\n>>,IS,0,start});
next(<<$\r,R/binary>>,{A,IS,_,_})->dents(R,{<<A/binary,$\r>>,IS,0,start});
next(<<4>>,{A,IS,_,_})->
  case dedents(A,IS,0) of {A2,_}->{ok,{A2,IS,0,start}};E->E end;
next(<<C,R/binary>>,{A,IS,_,_})->next(R,{<<A/binary,C>>,IS,0,inline}).
