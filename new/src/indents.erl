%% Run indent/detent algorithm and place appropriate strings in place for the
%% PEG grammar to use.
%%
%% From Python:
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
%%
%% This module replaces the first ws of any indent with 

-module(indents).


dents(Data) ->
  dents(Data, {[], [], 0, line_start}).

dents(Data, State) when is_list(Data) ->
  dents(erlang:iolist_to_binary(Data), State);

% dents(Data, {Acc, IStack, CurrILvl, State})
dents(Data, {_,_,_, inline} = State) ->
  to_nextline(Data, State);
dents(<<32, Rest/binary>>, {Acc, IS, CIL, _}) ->
  dents(Rest, {[32|Acc], IS, CIL+1, line_start});
dents(<<9, Rest/binary>>, {Acc, IS, CIL, _}) ->
  dents(Rest, {[9|Acc], IS, CIL+2, line_start});
% Newlines before other characters- throw away current indent level
dents(<<"\r\n", Rest/binary>>, {Acc, IS, _, _}) ->
  dents(Rest, {["\r\n" | Acc], IS, 0, line_start});
dents(<<"\n", Rest/binary>>, {Acc, IS, _, _}) ->
  dents(Rest, {["\n" | Acc], IS, 0, line_start});
dents(<<"\r", Rest/binary>>, {Acc, IS, _, _}) ->
  dents(Rest, {["\r" | Acc], IS, 0, line_start});
% Same indent level as last.  Moving along...
dents(<<C, Rest/binary>>, {Acc, [Last | _] = IS, Last, _}) ->
  to_nextline(Rest, {[C | Acc], IS, 0, inline});

% New indent level - indented more
dents(<<C, Rest/binary>>, {[WS| Acc], [Last | _] = IS, CIL, _}) when CIL > Last ->
  % replace last whitespace w/ indent token, push CIL onto stack, ...
  to_nextline(Rest, {




%% Skip ahead to right after the next newline
to_nextline(<<>>, {Acc, _, _, _} = State) ->
  {ok, lists:reverse(Acc), {[], 0, inline}};
to_nextline(<<"\r\n", Rest/binary>>, {Acc, IS, _, _}) ->
  dents(Rest, {["\n\r" | Acc], IS, 0, line_start});
to_nextline(<<"\n", Rest/binary>>, {Acc, IS, _, _}) ->
  dents(Rest, {["\n" | Acc], IS, 0, line_start});
to_nextline(<<"\r", Rest/binary>>, {Acc, IS, _, _}) ->
  dents(Rest, {["\r" | Acc], IS, 0, line_start});
to_nextline(<<C, Rest/binary>>, {Acc, IS, _, _}) ->
  to_nextline(Rest, {[C|Acc], IS, 0, inline}).
