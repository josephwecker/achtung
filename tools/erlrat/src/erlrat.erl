-module(erlrat).
-export([ast_to_matchfuns/1]).


-record(attrs, {
    notp=false,
    andp=false,
    star=false,
    plus=false,
    opt =false,
    tok =false,
    drop=false
  }).

ast_to_matchfuns([]) ->
  error_logger:error_msg("Empty grammar- parse it yourself.",[]);
ast_to_matchfuns(AST) ->
  % Turn it into a dictionary
  {Defs, EntryPoints} = make_defs(AST, dict:new(), []),
  % Make sure there's at least one entry-point
  EPs = case EntryPoints of []->[{N,_,_,_}|_]=AST,[N];_->EntryPoints end,
  % Scan for recursive rules & entry-points
  TopLevels = scan_tls(EPs, Defs),
  % Take toplevels and build full versions
  %Combined = [combine_rules(TL,Defs,TopLevels)||{TL,true}<-TopLevels:to_list()],
  Combined = combine_rules('%object',Defs,TopLevels),
  %Bin = term_to_binary(Combined),
  %file:write_file("output.bin",Bin).

  % TODO: YOU ARE HERE - creating stuff that's way too big.  Either figure out
  % automatically some nice additional top-levels or jump in and pull out the
  % star clauses, or make all non-token rules top-level...
  %
  %  * Star expressions
  %  * Any rule that has a parse transform
  %
  %  ... or
  %  maybe I should look at transformations I want to make first...

  Combined.

% Create general purpose lookup dictionary for the rules
make_defs([],Defs,EPs) -> {Defs,EPs};
make_defs([{{'ENTRY',Name},TrP,TrE,Body}|R],Defs,EPs) ->
  make_defs(R,Defs:store(Name,{TrP,TrE,Body}),[Name|EPs]);
make_defs([{Name,TrP,TrE,Body}|R],Defs,EPs) ->
  make_defs(R,Defs:store(Name,{TrP,TrE,Body}),EPs).

% Add entrypoints to toplevel and scan rules for recursion (which implies they
% need to be entrypoints as well).
scan_tls(EPs,Defs) -> scan_tls(EPs,Defs,dict:new()).
scan_tls([],_Defs,Tops) -> Tops;
scan_tls([EP|R],Defs,Tops) ->
  {_TrP,_TrE,Body} = Defs:fetch(EP),
  Tops2 = Tops:store(EP,true),
  Tops3 = scan_for_tops(Defs,Body,[],Tops2),
  scan_tls(R,Defs,Tops3).

scan_for_tops(Defs,{rule,_,Name},Seen,Tops) ->
  case dict:is_key(Name,Tops) of
    true -> Tops;
    false -> case lists:member(Name,Seen) of
        true -> Tops:store(Name,true);
        false ->
          case Defs:find(Name) of
            error -> error_logger:error_msg("Couldn't find a definition for ~p",[Name]);
            {ok, {_,_,Body}} -> scan_for_tops(Defs,Body,[Name|Seen],Tops)
          end
      end
  end;
scan_for_tops(Defs,{_,_,Exprs},Seen,Tops) when is_list(Exprs) ->
  lists:foldl(fun(Body,OldTops) -> scan_for_tops(Defs,Body,Seen,OldTops) end,
    Tops, Exprs);
scan_for_tops(_,_,_,Tops) -> Tops.

combine_rules(TLName, Defs, TLs) ->
  {_TrP,_TrE,Body} = Defs:fetch(TLName),
  {TLName, mform(Body,Defs,TLs,succ,fail)}.

% mform(Body, Defs, TopLevels, Succ, Fail) -> {m,Type,Succ,Fail}
mform({any,Attrs},_,_,S,F)             -> {m,{lit,Attrs,any},S,F};
mform({lit,_,_}=L,_,_,S,F)             -> {m,L,S,F};
mform({char,_,[]},_,_,_S,F)            -> F;
mform({char,Attrs,[Range|R]},D,T,S,F)  -> {m,{lit,Attrs,Range},S,mform({char,[],R},D,T,S,F)};
mform({ord,_,[]},_,_,_S,F)             -> F;
mform({ord,_Attrs,[Choice|R]},D,T,S,F) ->
  % TODO: need to process Attrs here
  mform(Choice,D,T,S,mform({ord,[],R},D,T,S,F));
mform({seq,_,[]},_,_,S,_F)             -> S;
mform({seq,_Attrs,[Expr|R]},D,T,S,F)   ->
  % TODO: need to process Attrs here
  mform(Expr,D,T,mform({seq,[],R},D,T,S,F),F);
mform({rule,Attrs,N},D,T,S,F)          ->
  case T:is_key(N) of
    true -> {m,{redirect,Attrs,N},S,F};
    false ->
      % TODO: if TrP or TrE are not empty- have them modify S&F
      {_TrP,_TrE,Body} = D:fetch(N),
      mform(Body,D,T,S,F)
  end.
