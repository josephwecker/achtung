%------------ Neotoma unpacking ----------------------------------------------
-define(X, []).              % Consume but ignore
-define(N, Node).
-define(I, Index).

-define(p1, ?p1(?N)).
-define(p2, ?p2(?N)).
-define(p3, ?p3(?N)).
-define(p4, ?p4(?N)).
-define(flat, ?flat(?N)).
-define(p1(L), lists:nth(1,L)).
-define(p2(L), lists:nth(2,L)).
-define(p3(L), lists:nth(3,L)).
-define(p4(L), lists:nth(4,L)).
-define(ip1, ?ip1(?N)).
-define(ip2, ?ip2(?N)).
-define(ip1(L), [?p1(IL)||IL<-L]).
-define(ip2(L), [?p2(IL)||IL<-L]).
-define(flat(L), lists:flatten(L)).
-define(all(Key,L), proplists:get_all_values(Key,?flat(L))).
-define(listify(V), case V of [_|_]->V;_->[V] end). % Ensure it's a list



% Node unpacking
%-define(GET(Key),all(Key, Node)).
%-define(RM(C),   rm_char(C, ?flat(Node),[])).
-define(L,       line(Index)).     % Current line #
% Used for literals  -- TODO: Move to ungbar.hrl
-define(BASE,    base(Node)).
-define(FIXNUM,  fix_num(Node, Index)).
-define(S2L(X),  literal(?flat(X),Index)). % Basically let Erlang turn it into a literal
-define(S2LIT,   ?S2L(Node)).

%-define(parse(Str),
%  begin
%      {ok, Tmp, 

% Erlang AST building helpers
-define(E,erl_syntax).

% Simple atomic terms
-define(a(Name), ?E:atom(Name)).
-define(v(Name), ?E:variable(Name)).
-define(l(List), ?E:list(List)).
-define(i(Int),  ?E:integer(Int)).
-define(module(Name),
  ?E:attribute(?a(module),[?a(Name)])).
-define(module(Name,Params),
  ?E:attribute(?a(module), [?a(Name), ?l([?v(P) ||P<-Params])])).
%-define(export_def(Module, Name, Arity), ...).
-define(export_def(Name, Arity),
  ?E:arity_qualifier(?a(Name), ?i(Arity))).
-define(export_list(ExpTups),
  ?E:attribute(?a(export),
    [?l([?export_def(N,A)||{N,A}<-ExpTups])])).
-define(forms(FormList), [?E:revert(F)||F<-FormList]).

% Append V onto L only if V has something
append_(L,V)  -> case V of [] -> L; _ -> lists:append(L,[V]) end.
literal(X, Index)  ->
  % TODO: pass along any errors here as appropriate
  {ok, Lit, _EndLoc} = erl_scan:string(X, line(Index)),
  case Lit of
    [Single] -> Single;
    M when is_list(M) -> M
      % much more serious- an actual list- which I haven't implemented yet.
      %[T || <-M, 
      %Multiple
  end.
rm_char(C, L)            -> rm_char(C, L, []).
rm_char(_, [], Acc)      -> lists:reverse(Acc);
rm_char(C, [C | T], Acc) -> rm_char(C, T, Acc);
rm_char(C, [H | T], Acc) -> rm_char(C, T, [H | Acc]).
%fixlstr(LStr) ->

