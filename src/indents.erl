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
    bigs = [],    % Block Ignore Stack
    it =   "<<indent>>",    % Indent Token
    dt =   "<<dedent>>",    % Dedent Token
    xt =   true,  % Extra indentation doesn't count as indentation
    stdi = auto,  % Standard indentation length
    igb =  ?DEFAULT_IGN_BLOCKS, % Ignore blocks definition list
    igb_fun,      % Processed Ignore Block definitions
    st =   inbol % Inner processing state
  }).
-define(EOF, 4).
-define(N1,  $\n).
-define(N2,  "\r\n").
-define(N3,  $\r).

-define(NXT(Byte, Rest), <<Byte, Rest/binary>>).
-define(ADD(Acc, ToAdd), list_to_binary([Acc, ToAdd])).
-define(S, S#s).

%-----------------------------------------------------------------------------

% TODO: test continuations by reading specific # of bytes from file instead of
% entire lines.
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
      false -> fun(_)->false end
    end,
  bin_matcher([Start],
    {IsEscaped, bin_matcher([End]), Cont, Ign},
    create_ignoreblock_fun(T));
create_ignoreblock_fun([]) -> fun(_)->false end.

bin_matcher(M) ->
  B = iolist_to_binary(M),
  L = byte_size(B),
  fun(<<A:L/binary,R/binary>>) when A=:=B->{true,B,R};(_)->false end.

bin_matcher(M, Succ, FailFun) ->
  B = iolist_to_binary(M),
  L = byte_size(B),
  fun(<<A:L/binary,R/binary>>) when A=:=B->{Succ,B,R};(C)->FailFun(C) end.


%-----------------------------------------------------------------------------

dents(<<>>,S)                 -> {cont,S};
dents(<<?EOF,_R/binary>>,S)   -> eof(S);
dents(Data, #s{st=inbol}=S)   -> inbol(   Data, ?S.a, S);
dents(Data, #s{st=inline}=S)  -> inline(  Data, ?S.a, S);
dents(Data, #s{st=inblock}=S) -> inblock( Data, ?S.a, ?S.bigs, [], S).

% At beginning of line and calculating current indent level
inbol(<<>>,A,S)        -> {cont,?S{a=A, st=inbol}}; % Continue
inbol(?NXT(?EOF,_),A,S)-> eof(?S{a=A});              % Finished
inbol(?NXT(32,R),A,S)  -> inbol(R,?ADD(A,32), ?S{ci=?S.ci+1}); % Inc indent
inbol(?NXT( 9,R),A,S)  -> inbol(R,?ADD(A, 9), ?S{ci=?S.ci+2});
inbol(?NXT(?N1,R),A,S) -> inbol(R,?ADD(A,?N1),?S{ci=0}); % Blank- start over
inbol(?NXT(?N2,R),A,S) -> inbol(R,?ADD(A,?N2),?S{ci=0});
inbol(?NXT(?N3,R),A,S) -> inbol(R,?ADD(A,?N3),?S{ci=0});
inbol(Dat,A,S) -> % Started some text- possibly do indent/dedent
  IsBlock = ?S.igb_fun,
  case IsBlock(Dat) of
    {{_,_,_,Ign}=BlockDef, StartTok, R} -> % Block is starting
      {A2, S2} = case Ign of % But before we go into block mode...
        false -> do_dent(A, ?S.ci, ?S.ds, ?S.stdi, ?S.xt, S);
        true -> {A,S} % Ignore indent
      end,
      inblock(R, ?ADD(A2, StartTok), [BlockDef], [], S2);
    false -> % Normal indent/dedent
      {A2, S2} = do_dent(A, ?S.ci, ?S.ds, ?S.stdi, ?S.xt, S),
      inline(Dat, A2, S2)
  end.

% Ignore, indent, set first indent, or dedent as appropriate
do_dent(A,CI,[CI|_],_,_,S)->{A, S};
do_dent(A,CI,[0],auto,_,S)->{?ADD(A, ?S.it),?S{ds=[CI,0],stdi=(CI*2)}};
do_dent(A,CI,[L|_],StdI,true,S) when (CI - L) >= StdI -> {A,S};
do_dent(A,CI,[L|_],_,_,S) when CI > L -> {?ADD(A, ?S.it),?S{ds=[CI|?S.ds]}};
do_dent(A,CI,[L|_],_,_,S) when CI < L -> dedents(A,?S.ds,CI,S).

% Emit the correct number of dedents
dedents(A,[],CI,_S) -> throw({indent_error, A, none, CI});
dedents(A,[L|_]=DS,L,S) -> {A, ?S{ds=DS}};
dedents(A,[L|_],CI,_S) when CI > L -> throw({indent_error, A, L, CI});
dedents(A,[_|R],CI,S) -> dedents(?ADD(A, ?S.dt), R, CI, S).

% Going through the rest of the line
inline(<<>>,A,S)        -> {cont, ?S{a=A, st=inline}};
inline(?NXT(?EOF,_),A,S)-> eof(?S{a=A});
inline(?NXT(?N1,R),A,S) -> inbol(R,?ADD(A,?N1),?S{ci=0});
inline(?NXT(?N2,R),A,S) -> inbol(R,?ADD(A,?N2),?S{ci=0});
inline(?NXT(?N3,R),A,S) -> inbol(R,?ADD(A,?N3),?S{ci=0});
inline(Dat,A,S) ->
  IsBlock = ?S.igb_fun,
  case IsBlock(Dat) of
    {BD, Toks, D2} -> inblock(D2, ?ADD(A,Toks), [BD], [], S);
    false -> ?NXT(C,R) = Dat, inline(R, ?ADD(A,C), S)
  end.

%inblock(D,A,[{EscMF, EndMF, Contains, Ignored}|R]=BlockStack,LastChr,S) ->
inblock(D,A,[],<<$\n>>,S)    -> inbol(D,A,?S{bigs=[], ci=0});
inblock(D,A,[],<<$\r>>,S)    -> inbol(D,A,?S{bigs=[], ci=0});
inblock(D,A,[],_,S)          -> inline(D,A,?S{bigs=[]});
inblock(<<>>,A,BS,_,S)       -> {cont, ?S{a=A, st=inblock, bigs=BS}};
inblock(?NXT(?EOF,_),A,_,_,S)-> eof(?S{a=A}); % Parser's problem- not ours
inblock(Dat,A,[{EscMF, EndMF, Contains, _}|R]=BS, _, S) ->
  case EscMF(Dat) of
    {true,Toks,D2} -> inblock(D2,?ADD(A,Toks),BS,[],S); % Moving along
    false ->
      case {EndMF(Dat), Contains} of
        {{true,Toks,D2},_}->inblock(D2,?ADD(A,Toks),R,Toks,S); % Done with this block
        {false, false} ->
          ?NXT(C,D2) = Dat,inblock(D2,?ADD(A,C),BS,[],S); % Moving along
        {false, true} ->
          IsBlock = ?S.igb_fun,
          case IsBlock(Dat) of
            {NewBDef, Toks, D2} ->
              inblock(D2,?ADD(A,Toks),[NewBDef|BS],[],S); % Block within a block
            false -> ?NXT(C,D2) = Dat, inblock(D2,?ADD(A,C),BS,[],S) % Moving along
          end
      end
  end.

eof(S) ->
  {A2, _S2} = dedents(?ADD(?S.a,"\n"), ?S.ds, 0, S),
  {ok, A2}.
