%% Run indent/detent algorithm and inject indent and dedent tokens (9 and 21)
%% when appropriate.  Tabs are treated as 2 spaces.  An EOF (<<4>>) causes it
%% to put on any remaining dedents it needs to.
%%
%% If the source-code seems pretty dense, don't forget to compile with the 'P'
%% flag and look at the expanded, prettified output.
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
%% Put indent/dedent tokens right before newline before they occur (if
%% possible)- it'll ensure that line/col stuff in error messages will be
%% accurate and might simplify parsing...
%%


-module(indents).
-export([file_scan/1, file_scan/2,
         full_scan/1, full_scan/2,
         scan/1,      scan/2]).
-define(DEFAULT_IGN_BLOCKS, [
    % Type     Start   End     Esc?    Contain? Total-ignore?
    {string,   "\"",   "\"",   true,   false,   false},
    {comment,  "#",    "\n",   false,  false,   true},
    {args,     "(",    ")",    false,  true,    false},
    {selector, "[",    "]",    false,  true,    false}]).
-record(s, {
    a =    <<>>,  % Accumulator
    ds =   [0],   % Dent Stack
    ci =   0,     % Current Indent Level
    igs =  [],    % Ignore Stack
    it =   "<<indent>>",    % Indent Token
    dt =   "<<dedent>>",    % Dedent Token
    xt =   true,  % Extra indentation doesn't count as indentation
    stdi = auto,  % Standard indentation length
    igb =  ?DEFAULT_IGN_BLOCKS, % Ignore blocks definition list
    igb_fun,      % Processed Ignore Block definitions
    st =   active % Inner processing state
  }).
-define(EOF, 4).
-define(N1,  $\n).
-define(N2,  "\r\n").
-define(N3,  $\r).

-define(NXT(Byte, Rest), <<Byte, Rest/binary>>).
-define(ADD(Acc, ToAdd), list_to_binary([Acc, ToAdd])).
-define(S, S#s).

%-----------------------------------------------------------------------------

file_scan(FName)-> file_scan(FName, []).
file_scan(FName, Opts) ->
  % We're going to be nice and not pull it all into memory to begin with.
  case file:open(FName, [raw, binary, {read_ahead, 512}]) of
    {ok, File} -> do_file_scan(File, 1, new_state(Opts));
    Other -> Other
  end.

full_scan(Data) -> full_scan(Data, []).
full_scan(Data, Opts)-> scan([Data,<<?EOF>>], Opts).

scan(Data) -> scan(Data, []).
scan(Data, Opts) when is_list(Data)  -> scan(erlang:iolist_to_binary(Data), Opts);
scan(Data, Opts) when is_binary(Data)-> dents(Data, new_state(Opts));
scan(Data, Cont) when is_tuple(Cont) -> dents(Data, Cont).

%-----------------------------------------------------------------------------

do_file_scan(File, L, S) ->
  case file:read_line(File) of
    eof -> case dents(<<?EOF>>,S) of {ok,S2}->{ok,S2}; O->{L,O} end;
    {ok,Dat}->case dents(Dat,S) of
        {cont, S2} -> do_file_scan(File, L+1, S2);
        {ok, Result} -> {ok, Result};
        O->{L,O} end;
    Other -> {L,Other}
  end.

new_state(Opts) -> new_state(Opts, #s{}).
new_state([], S) -> ?S{igb_fun=(create_ignoreblock_fun(?S.igb))};
new_state([{indent_token,        V}|T], S) -> new_state(T, ?S{it=V});
new_state([{dedent_token,        V}|T], S) -> new_state(T, ?S{dt=V});
new_state([{extra_indent_ignored,V}|T], S) -> new_state(T, ?S{xt=V});
new_state([{ignoreblock_defs,    V}|T], S) -> new_state(T, ?S{igb=V});
new_state([O|T], S) ->
  error_logger:warning("Unsupported indents option \"~p\". Ignoring.", [O]),
  new_state(T, S).

create_ignoreblock_fun([{_,Start,End,Esc,Cont,Ign}|T]) ->
  % One function returns false or
  %  {{Escaped matcher, End matcher, Contains?, Ignore?}, Rest}
  IsEscaped = case Esc of
      true -> bin_matcher(["\\",[End]]);
      false -> fun(B)->{false,B} end
    end,
  bin_matcher([Start],
    {IsEscaped, bin_matcher([End]), Cont, Ign},
    create_ignoreblock_fun(T));
create_ignoreblock_fun([]) -> fun(_)->false end.

bin_matcher(M) ->
  B = iolist_to_binary(M),
  L = byte_size(B),
  fun(<<A:L/binary,R/binary>>) when A=:=B->{true,B,R};(C)->{false,C} end.

bin_matcher(M, Succ, FailFun) ->
  B = iolist_to_binary(M),
  L = byte_size(B),
  fun(<<A:L/binary,R/binary>>) when A=:=B->{Succ,B,R};(C)->FailFun(C) end.


%-----------------------------------------------------------------------------

dents(<<>>,S) -> {cont,S};
dents(<<?EOF,_R/binary>>,S) -> eof(S);
dents(Data, #s{st=active}=S) -> active(Data, ?S.a, S);
dents(Data, #s{st=inline}=S) -> inline(Data, ?S.a, S).
% TODO - other continue states

% active - calculating current indent level
% Finish
active(<<>>,A,S)        -> {cont,?S{a=A, st=active}};
active(?NXT(?EOF,_),A,S)  -> eof(?S{a=A});
% Increment indent level
active(?NXT(32,R),A,S)    -> active(R,?ADD(A,32), ?S{ci=?S.ci+1});
active(?NXT( 9,R),A,S)    -> active(R,?ADD(A, 9), ?S{ci=?S.ci+2});
% Blank line- start over
active(?NXT(?N1,R),A,S)   -> active(R,?ADD(A,?N1),?S{ci=0});
active(?NXT(?N2,R),A,S)   -> active(R,?ADD(A,?N2),?S{ci=0});
active(?NXT(?N3,R),A,S)   -> active(R,?ADD(A,?N3),?S{ci=0});
% Started some text- possibly do indent/dedent
active(Dat,A,S) ->
  IsBlock = ?S.igb_fun,
  case IsBlock(Dat) of
    {{_,_,_,Ign}=BlockDef, StartTok, R}  ->
      % Inside a block- do indent/dedent unless ignored by this kind of block,
      % and then start running in block-mode.
      {A2, S2} = case Ign of
        false -> do_dent(A, ?S.ci, ?S.ds, ?S.stdi, ?S.xt, S);
        true -> {A, S}
      end,
      launchblock(R,?ADD(A2, StartTok),BlockDef,S2);
    false ->
      {A2, S2} = do_dent(A, ?S.ci, ?S.ds, ?S.stdi, ?S.xt, S),
      inline(Dat, A2, S2)
  end.

% Same level as last- nothing to do
do_dent(A, CI, [CI|_], _, _, S) ->
  {A, S};
% First indent of the day
do_dent(A, CI, [0], auto, _, S) ->
  {?ADD(A, ?S.it), ?S{ds=[CI,0], stdi=(CI*2)}};
% Ignored indent - Don't insert a token or modify dent-stack
do_dent(A, CI, [L|_], StdI, true, S) when (CI - L) >= StdI ->
  {A, S};
% Indent that's good to go!
do_dent(A, CI, [L|_], _, _, S) when CI > L ->
  {?ADD(A, ?S.it), ?S{ds=[CI|?S.ds]}};
% Dedent that's good to go
do_dent(A, CI, [L|_], _, _, S) when CI < L ->
  dedents(A, ?S.ds, CI, S).

dedents(A,[],CI,_S) -> throw({indent_error, A, none, CI});
dedents(A,[L|_]=DS,L,S) -> {A, ?S{ds=DS}};
dedents(A,[L|_],CI,_S) when CI > L -> throw({indent_error, A, L, CI});
dedents(A,[_|R],CI,S) -> dedents(?ADD(A, ?S.dt), R, CI, S).

% BlockDef = {EscMF, EndMF, Contains, Ignored}
launchblock(Dat, Acc, _BlockDef, S) ->
  io:format("~n---~nStarting a block: ~p | ~p~n---~n~n", [Acc, Dat]),
  % TODO: put new block on stack and start running through...
  {cont, ?S{a=Acc}}.

inline(<<>>,A,S) -> {cont, ?S{a=A, st=inline}};
inline(?NXT(?EOF,_),A,S) -> eof(?S{a=A});
inline(?NXT(?N1,R),A,S) -> active(R,?ADD(A,?N1),?S{ci=0});
inline(?NXT(?N2,R),A,S) -> active(R,?ADD(A,?N2),?S{ci=0});
inline(?NXT(?N3,R),A,S) -> active(R,?ADD(A,?N3),?S{ci=0});
% TODO: Check for blocks here
inline(?NXT(C,R),A,S) -> inline(R,?ADD(A,C),S).

% 
% block runs drop into inline runs unless last token was a newline

eof(S) ->
  {A2, _S2} = dedents(?ADD(?S.a,"\n"), ?S.ds, 0, S),
  {ok, A2}.
