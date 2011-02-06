-module(apg_to_parser).
-compile(export_all).


file(FName) -> process(ungpeg_n:file(FName)).
parse(Txt)  -> process(ungpeg_n:parse(Txt)).

process(AST) ->
  Defs = make_defs(AST, dict:new()),
  Defs.

make_defs([],Defs) -> Defs;
make_defs([{rule,Name,Attrs,{ExType,ExAttr,ExBody}}|R],Defs) ->
  make_defs(R,Defs:store(Name,give_uids({ExType,ExAttr++Attrs,ExBody}))).

give_uids({T,A,L}) when T==seq;T==pch;T==xch ->
  {T,[{uid,uid(T)}|A],lists:map(fun give_uids/1, L)};
give_uids({T,A,B}) -> {T,[{uid,uid(T)}|A],B}.

uid(Type) -> atom_to_list(Type)++"_"++chr_replace($.,"_",lists:nth(2,string:tokens(erlang:ref_to_list(make_ref()),"<>"))).
chr_replace(Needle,Replacement,Haystack) ->
  chr_replace(Needle,Replacement,Haystack,[]).
chr_replace(_,_,[],A)   ->lists:flatten(lists:reverse(A));
chr_replace(C,R,[C|T],A)->chr_replace(C,R,T,[R|A]);
chr_replace(C,R,[H|T],A)->chr_replace(C,R,T,[H|A]).
