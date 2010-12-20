% Neotoma helpers
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
%-define(p1(L), begin [R|_]=L, R end).
%-define(p2(L), begin [_|[R|_]]=L, R end).
%-define(p3(L), begin [_|[_|[R|_]]]=L, R end).
%-define(p4(L), begin [_|[_|[_|[R|_]]]]=L, R end).
-define(flat(L), lists:flatten(L)).
-define(all(Key,L), proplists:get_all_values(Key,?flat(L))).

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

