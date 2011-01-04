-module(test_erlrat).
-compile(export_all).


test()->
  [erlrat3:ast_to_matchfuns(ast(N))||N<-lists:seq(1,2)].

ast(1)-> [{'1',[],[],{rule,[],'1'}}];
ast(2)-> [{'1',[],[],{rule,[],'2'}},
          {'2',[],[],{rule,[],'2'}}];

ast(_)-> fail.
