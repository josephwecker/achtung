-module(test_erlrat).
-compile(export_all).


test()->
  [erlrat:ast_to_matchfuns(ast(N))||N<-lists:seq(1,4)].

ast(1)-> [{'1',[],[],{rule,[],'1'}}];
ast(2)-> [{'1',[],[],{rule,[],'2'}},
          {'2',[],[],{rule,[],'2'}}];
ast(3)-> [{'1',[],[],{rule,[],'3'}},
          {'2',[],[],{rule,[],'3'}},
          {'3',[],[],{rule,[],'2'}}];

ast(4)->
  [{'%SPACE', [], [],
      {char,[],[$ ,$\t]}},
    {'%

ast(_)-> fail.
