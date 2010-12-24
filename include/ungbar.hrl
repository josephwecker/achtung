%------------ Neotoma unpacking ----------------------------------------------
-define(X, []). % Consume but ignore
-define(N, Node).
-define(I, Index).
-define(inspect, ?inspect(?N)).
-define(inspect(N), begin io:format("~n====== inspect =======~n~p~n"
        "======================~n", [N]), N end).

-define(p1, ?p1(?N)).
-define(p2, ?p2(?N)).
-define(p3, ?p3(?N)).
-define(p4, ?p4(?N)).
-define(p5, ?p5(?N)).
-define(p6, ?p6(?N)).
-define(flat, ?flat(?N)).
-define(p1(L), lists:nth(1,L)).
-define(p2(L), lists:nth(2,L)).
-define(p3(L), lists:nth(3,L)).
-define(p4(L), lists:nth(4,L)).
-define(p5(L), lists:nth(5,L)).
-define(p6(L), lists:nth(6,L)).
-define(ip1, ?ip1(?N)).
-define(ip2, ?ip2(?N)).
-define(ip3, ?ip3(?N)).
-define(ip4, ?ip4(?N)).
-define(ip1(L), [?p1(IL)||IL<-L]).
-define(ip2(L), [?p2(IL)||IL<-L]).
-define(ip3(L), [?p3(IL)||IL<-L]).
-define(ip4(L), [?p4(IL)||IL<-L]).

% ?i(3,4) == ?ip3(?p4)
-define(i(IPos,OPos), [lists:nth(IPos, IL)||IL<-lists:nth(OPos,?N)]).
-define(flat(L), lists:flatten(L)).
-define(all(Key,L), proplists:get_all_values(Key,?flat(L))).
-define(listify, ?listify(?N)). % Ensure it's a list
-define(listify(V), case V of [_|_]->V;_->[V] end).
-define(app(L,V),case V of []->L;_->lists:append(L,[V]) end).% Append V onto L sometimes


%------------ Parse Tree Construction ---------------------------------------
-define(pos, line(Index)).
-define(scan,?scan(?N)).
-define(scan(V), % For simple terms that the erlang scanner can handle (most atomic literals)
  case erl_scan:string(?flat(V),?pos) of
    {ok, [One], _} -> One;
    {ok, Tokens, _} ->
      {ok, Parsed} = erl_parse:parse_term(Tokens ++ [{dot,?pos}]),
      erl_parse:abstract(Parsed);
    Err -> throw(Err)
  end).

-define(c_nil, {nil,?pos}).
-define(c_function(N,Clauses), {function,?pos,?v_atom(N),length(element(3,?p1(Clauses))),Clauses}).
-define(c_clause(Pattern, Guard, Body), {clause,?pos,Pattern,Guard,Body}).
-define(c_block(Exprs), {block, ?pos, Exprs}).
-define(c_tuple(Items), {tuple, ?pos, Items}).
-define(c_list(IT),case IT of []->?c_nil;{I,T}->l2c(I,T,?pos) end).
-define(c_atom(N),{atom,?pos,case N of [_|_]->?scan(N);_->N end}).
-define(v_atom(A), element(3,A)).
-define(c_fsig(NameParts,Arity),
  case lists:reverse(NameParts) of
    [One]     -> {'fun',?pos,{function,?v_atom(One),list_to_integer(Arity)}};
    [Fun,Mod] -> {'fun',?pos,{function,?v_atom(Mod),?v_atom(Fun),list_to_integer(Arity)}};
    [Fun|ModP]-> {'fun',?pos,{function,combine_atoms(ModP),?v_atom(Fun),list_to_integer(Arity)}}
  end).
% == Function / fun references ==
% toplevelfun() -> ...   -->  {function,P,toplevelfun,0,[{clause...},...]}
% fun myfun/0            -->  {'fun',P,{function,myfun,0}}
% fun lists:reverse/1    -->  {'fun',P,{function,lists,    reverse, 1}}
% fun pkg.mdl:some_fun/0 -->  {'fun',P,{function,'pkg.mdl',some_fun,0}}
% fun()->a end           -->  {'fun',P,{clauses,[...]}}  % (Note no arity mentioned)
% some_fun/2 (in exp/imp)-->  {some_fun, 2}


combine_atoms(Atoms) -> list_to_atom(string:join([atom_to_list(?v_atom(A))||A<-Atoms],".")).

% List to Conses - basically recursively (not tail recursively at the moment)
% takes a proper list with a tail (usually [] when the result is going to be
% proper) and turns it into nested cons tuples.
% Usage: l2c(ProperList, TailForEnd, Position)
l2c([H|R],T,Pos) ->
  {cons, Pos, H,
    case R of
      []->  % Done with main proper-list, time for tail
        case T of
          [] -> {nil,Pos};    % Final result is proper
          V  -> V             % Final result is improper
        end;
      [_|_] -> l2c(R, T, Pos) % Keep going deeper. I hope your stack loves you.
    end}.
