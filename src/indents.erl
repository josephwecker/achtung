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
    it =   <<"<<indent>>">>,    % Indent Token
    dt =   <<"<<dedent>>">>,    % Dedent Token
    xt =   true,  % Extra indentation doesn't count as indentation
    stdi = auto,  % Standard indentation length
    igb =  ?DEFAULT_IGN_BLOCKS, % Ignore blocks definition list
    igb_fun       % Processed Ignore Block definitions
    st =   active % Inner processing state
  }).
% States:
%  - active_calc   : scanning whitespace @ beginning of line for indent-level
%  - active_inline : not in an ignore block, but working toward next newline
%  - ignoring      : some level deep in ignore-blocks
-define(EOF, 4).
-define(N1,  $\n).
-define(N2,  "\r\n").
-define(N3,  $\r).

-define(A(Byte, Rest), <<Byte, Rest/binary>>).
-define(Z(Acc, Byte), <<Acc/binary, Byte>>).
-define(S, S#s).

%-----------------------------------------------------------------------------

file_scan(FName)-> file_scan(FName, []).
file_scan(FName, Opts) ->
  % We're going to be nice and not pull it all into memory to begin with.
  {ok, File} = file:open(FName, [raw, binary, {read_ahead, 512}]),
  do_file_scan(File, 1, new_state(Opts)).

full_scan(Data) -> full_scan(Data, Opts).
full_scan(Data, Opts)-> scan([Data,<<?EOF>>], Opts).

scan(Data) -> scan(Data, []).
scan(Data, Opts) when is_list(Data)  -> scan(erlang:iolist_to_binary(Data), Opts);
scan(Data, Opts) when is_binary(Data)-> dents(Data, new_state(Opts));
scan(Data, Cont) when is_tuple(Cont) -> dents(Data, Cont).

%-----------------------------------------------------------------------------

do_file_scan(File, L, S) ->
  case file:read_line(File) of
    eof     ->case dents(<<?EOF>>,S) of {ok,S2}->{ok,S2}; O->{L,O} end;
    {ok,Dat}->case dents(Dat,S) of {ok,S2}->do_file_scan(File,L+1,S2);O->{L,O} end;
    Other   ->{L,Other}
  end.

new_state(Opts) -> new_state(Opts, #s{}).
new_state([], S) -> create_ignoreblock_fun(S);
new_state([{indent_token,        V}|T], S) -> new_state(T, ?S{it=V});
new_state([{dedent_token,        V}|T], S) -> new_state(T, ?S{dt=V});
new_state([{extra_indent_ignored,V}|T], S) -> new_state(T, ?S{xt=V});
new_state([{ignoreblock_defs,    V}|T], S) -> new_state(T, ?S{igb=V});
new_state([O|T], S) ->
  error_logger:warning("Unsupported indents option \"~p\". Ignoring.", [O]),
  new_state(T, S).

create_ignoreblock_fun(#s{igb=IGB}=S) -> ?S{igb_fun=create_ignoreblock_fun(IGB,S)}.
create_ignoreblock_fun([{_,Start,End,Esc,Cont,Ign}|T], S) ->
  % One function returns false or
  %  {{Escaped matcher, End matcher, Contains?, Ignore?}, Rest}
  IsEscaped = case Esc of
      true -> bin_matcher(["\\",[End]]);
      false -> fun(B)->{false,B} end
    end,
  bin_matcher([Start],
    {IsEscaped, bin_matcher([End]), Cont, Ign},
    create_ignoreblock_fun(T, S)).
create_ignoreblock_fun([], S) -> fun(_)->false;

bin_matcher(M) ->
  B = iolist_to_binary(M),
  L = byte_size(B),
  fun(<<A:L/binary,R/binary>>) when A=:=B->{true,R};(C)->{false,C} end.

bin_matcher(M, Succ, FailFun) ->
  B = iolist_to_binary(M),
  L = byte_size(B),
  fun(<<A:L/binary,R/binary>>) when A=:=B->{Succ, R};(C)->FailFun(C) end.


%-----------------------------------------------------------------------------

dents(<<>>,S) -> {cont,S};
dents(<<?EOF,_R/binary>>,S) -> eof(S);
dents(Data, #s{st=active}=S) -> active(Data, ?S.a, S).
% TODO - other continue states

% active - calculating current indent level
% Finish
active(<<>>,A,S)        -> {cont,?S{a=A, st=active}};
active(?A(?EOF,_)),A,S) -> eof(?S{a=A});
% Increment indent level
active(?A(32,R),A,S)    -> active(R,?Z(A,32), ?S{ci=?S.ci+1};
active(?A( 9,R),A,S)    -> active(R,?Z(A, 9), ?S{ci=?S.ci+2};
% Blank line- start over
active(?A(?N1,R),A,S)   -> active(R,?Z(A,?N1),?S{ci=0});
active(?A(?N2,R),A,S)   -> active(R,?Z(A,?N2),?S{ci=0});
active(?A(?N3,R),A,S)   -> active(R,?Z(A,?N3),?S{ci=0});
active(Dat,A,S) ->
  case start_block
  % if block-start
  %   if block.total_ignore
  %     start block run
  %   else
  %     do indent
  %     start block run
  % else
  %   do indent
  %   start inline run
  % 
  % block runs drop into inline runs unless last token was a newline

% Same as last indent-level- moving along...
%active(?A(C,R), A, #s{ci=CI,ds=[CI|_]}=S) -> inline(R,?Z(A,C),S);
% Indent - first one ever
%active(?A(C,R), A, #s{ci=CI, ds=[0], stdi=auto}=S) ->
%  inline(R,?Z(A,C),?S{ci=0,ds=[CI,0],stdi=CI});
% Indent - extra-gets-ignored turned off
%active(?A(C,R), A, #s{ci=CI, ds=[L|_], xt=false}=S) when CI > L ->
