if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn keyword erlpegTodo            TODO FIXME XXX NOTE NOTES contained
syn match   erlpegComment         /#.*$/ contains=erlpegTodo,erlpegDoc,@Spell
syn region  erlpegBlockComment    start="#|" skip="\(\\#|\|\\|#\)" end="|#" contains=erlpegBlockComment,erlpegTodo,@Spell

" Rules


" Special rules
syn match erlpegEmpty   +empty\|''\|""\|&\s*(\s*any\s*/\s*eof)\|&\s*(\s*eof\s*/\s*any)+
syn match erlpegAny     +any\|\.\|!\s*eof+
syn match erlpegEof     +eof\|!\s*\.\|!\s*any+


" Literals (strings)
syn match   erlpegLitModifier     /\\\^[a-zA-Z]\|\\./
syn match   erlpegLitModifier     /\\x{[a-fA-F0-9]*}\|\\x[a-fA-F0-9]\{2\}/ contained contains=erlpegNum
syn match   erlpegLitModifier     /\\[0-7]\{1,3\}/ contained contains=erlpegOctNum
syn region  erlpegDoubleLit       start=+"+  skip=+\\"+  end=+"+ contains=@Spell,erlpegLitModifier
syn region  erlpegSingleLit       start=+'+  skip=+\\'+  end=+'+ contains=@Spell,erlpegLitModifier
syn match   erlpegNum             /[a-fA-F0-9]*/ contained
syn match   erlpegOctNum          /[0-7]\{1,3\}/ contained

" Rule
syn match   erlpegEntryPoint      /</ contained
syn match   erlpegRuleName        /\(^\|;\)\s*<\?[a-zA-Z_]\+/ contains=erlpegEntryPoint
syn match   erlpegAssign          /<-/

" Ordered
syn match   erlpegOrdSep          /\//
" Seq
syn match   erlpegSeqSep          />/
" Group
syn match   erlpegGroup           /[()]/
" prefixes
syn match   erlpegPrefix          /[!&]/
" suffixes
syn match   erlpegSuffix          /[\*+?]/

" calls
" tokens
" ranges
" tags
" any/eof/empty
" transforms
"   position vars
"   named vars
"   tagged vars
"   lists
"   tuples
"   transform calls

hi link erlpegDoubleLit           String
hi link erlpegSingleLit           String
hi link erlpegLitModifier         SpecialChar
hi link erlpegNum                 Underlined
hi link erlpegOctNum              Underlined
hi link erlpegRuleName            Identifier
hi link erlpegEntryPoint          Typedef
hi link erlpegAssign              Operator
hi link erlpegOrdSep              Structure
hi link erlpegSeqSep              Structure
hi link erlpegGroup               Delimiter
hi link erlpegPrefix              PreProc
hi link erlpegSuffix              Repeat

let b:current_syntax = "erlpeg"
