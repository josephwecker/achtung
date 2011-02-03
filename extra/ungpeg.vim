if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" ---------------------------------
" I know I know- not the right place for this stuff. Just temporary.
set ambiwidth=double
" epsilon: Empty string / success
map! \eps ɛ
map! \succ ɛ
map! \empty ɛ
" digamma: failure / !ɛ
map! \fail ϝ
map! \dig ϝ

map! <- ←
map! -> →
map! // ⑊
map! <<< 《
map! << 〈
map! >>> 》
map! >> 〉

" ---------------------------------

syn sync fromstart
syn sync linebreaks=1

syn keyword erlpegTodo            TODO FIXME XXX NOTE NOTES contained
syn match   erlpegComment         /#.*$/ contains=erlpegTodo,erlpegDoc,@Spell
syn region  erlpegBlockComment    start="#|" skip="\(\\#|\|\\|#\)" end="|#" contains=erlpegBlockComment,erlpegTodo,@Spell

syn match   erlpegLitModifier     /\\\^[a-zA-Z]\|\\./ contained
syn match   erlpegLitModifier     /\\x{[a-fA-F0-9]*}\|\\x[a-fA-F0-9]\{2\}/ contained contains=erlpegNum
syn match   erlpegLitModifier     /\\[0-7]\{1,3\}/ contained contains=erlpegOctNum
syn region  erlpegDoubleLit       start=+"+ skip=+\\.+ end=+"+ keepend contains=erlpegDLitDelim,@Spell,erlpegLitModifier
syn region  erlpegSingleLit       start=+'\|‹+ skip=+\\.+ end=+'\|›+ keepend contains=@Spell,erlpegLitModifier,erlpegSLitDelim
syn match   erlpegNum             /[a-fA-F0-9]*/ contained
syn match   erlpegOctNum          /[0-7]\{1,3\}/ contained
syn match   erlpegDLitDelim       /\\\@!"/ contained
syn match   erlpegSLitDelim       /\\\@!\('\|‹\|›\)/ contained


syn match   erlpegToken           /\<[A-Z][a-zA-Z0-9_:]*/ contains=erlpegTaggedRule
syn match   erlpegRuleRef         /\<[a-z][a-zA-Z0-9_:]*/ contains=erlpegTaggedRule
syn match   erlpegOptToken        /\<[A-Z][a-zA-Z0-9_:]*?/ contains=erlpegTaggedRule
syn match   erlpegOptRuleRef      /\<[a-z][a-zA-Z0-9_:]*?/ contains=erlpegTaggedRule
syn match   erlpegTaggedRule      /:[a-zA-Z0-9_]*/ contained

" Rule
syn match   erlpegEntryRule       /\(^\|;\)\s*:[a-z][a-zA-Z0-9_]*/ contains=erlpegEntryDelim
syn match   erlpegNormRuleName    /\(^\|;\)\s*[a-z][a-zA-Z0-9_]*/
syn match   erlpegTokRuleName     /\(^\|;\)\s*[A-Z][a-zA-Z0-9_]*/
syn match   erlpegAssign          /<\(-\|=\)\+\|←/

syn match   erlpegEntryDelim      /:/ contained
syn match   erlpegOrdSep          /\//
syn match   erlpegXordSep         /\/\/\|║\|⑊/
syn match   erlpegSeqSep          /,/
syn match   erlpegGroup           /[()]/
syn match   erlpegPrefix          /[!&]/
syn match   erlpegSuffix          /[\*+]/
syn match   erlpegOptSuffix       /?/

syn match   erlpegCollapseFun     /-\?{[^}]*}/ contains=erlpegExecDelim "contains=erlpegTransAtom
syn match   erlpegTransFun        /-\?{|[^|]*|}/ contains=erlpegExecDelim "contains=erlpegTransAtom
syn match   erlpegCollapseFun2    /〈[^〉]*〉/ contains=erlpegExecDelim "contains=erlpegTransAtom
syn match   erlpegTransFun2       /《[^》]*》/ contains=erlpegExecDelim "contains=erlpegTransAtom

syn match   erlpegExecDelim       /\\\@!\({\|}\)/ contained

" Special rules
syn match erlpegEmpty   +ɛ\|\<succ\>\|\<empty\>\|''\|""\|&\s*(\s*any\s*/\s*eof)\|&\s*(\s*eof\s*//\?\s*any)+
syn match erlpegAny     +\<any\>\|\.\|!\s*eof+
syn match erlpegEOF     +\<eof\>\|!\s*\.\|!\s*any+
syn match erlpegFail    +ϝ\|fail+

" Ranges
syn match   erlpegAllCharMods     /\\[0-7]\{1,3\}\|\\x{[a-fA-F0-9]*}\|\\x[a-fA-F0-9]\{2\}\|\\\^[a-zA-Z]\|\\./ contained
syn match erlpegRanges  /\[\(\(\\[0-7]\{1,3\}\|\\x{[a-fA-F0-9]*}\|\\x[a-fA-F0-9]\{2\}\|\\\^[a-zA-Z]\|\\.\|.\)-\(\\[0-7]\{1,3\}\|\\x{[a-fA-F0-9]*}\|\\x[a-fA-F0-9]\{2\}\|\\\^[a-zA-Z]\|\\.\|[^\]]\)\)*\(\\[0-7]\{1,3\}\|\\x{[a-fA-F0-9]*}\|\\x[a-fA-F0-9]\{2\}\|\\\^[a-zA-Z]\|\\.\|[^\]]\)*\]/ contains=erlpegLitModifier,erlpegRange,erlpegRangeSpec2
syn match erlpegRange   /\[\(\(\\[0-7]\{1,3\}\|\\x{[a-fA-F0-9]*}\|\\x[a-fA-F0-9]\{2\}\|\\\^[a-zA-Z]\|\\.\|.\)-\(\\[0-7]\{1,3\}\|\\x{[a-fA-F0-9]*}\|\\x[a-fA-F0-9]\{2\}\|\\\^[a-zA-Z]\|\\.\|[^\]]\)\)*/ contains=erlpegLitModifier,erlpegRangeSpec1,erlpegRangeDelim contained
syn match erlpegRangeDelim /-/ contained
syn match erlpegRangeSpec1 /\[/ contained
syn match erlpegRangeSpec2 /\]/ contained

syn match erlpegTransform /\(->\|→\).*/ contains=erlpegTransMacro,erlpegTransPosMacro,erlpegTransAtom,erlpegTransDelim,erlpegComment
syn match erlpegTransAtom  /[a-zA-Z_][a-zA-Z0-9_]*/ contained
syn match erlpegTransMacro /\$[A-Za-z_]\+/ contained
syn match erlpegTransPosMacro /\$[1-9][0-9]*/ contained
syn match erlpegTransDelim    /->\|→/ contained

hi link erlpegComment             Comment
hi link erlpegBlockComment        Comment
hi link erlpegTodo                SpecialComment
hi link erlpegDoubleLit           String
hi link erlpegSingleLit           String
hi link erlpegLitModifier         SpecialChar
hi link erlpegNum                 Underlined
hi link erlpegOctNum              Underlined
hi link erlpegEntryRule           Label
hi link erlpegNormRuleName        Identifier
hi link erlpegTokRuleName         Debug
hi link erlpegEntryPoint          Typedef
hi link erlpegEntryDelim          Delimiter
hi link erlpegAssign              Delimiter
hi link erlpegOrdSep              Structure
hi link erlpegXordSep             Structure
hi link erlpegSeqSep              Structure
hi link erlpegGroup               Delimiter
hi link erlpegPrefix              PreProc
hi link erlpegSuffix              Repeat
hi link erlpegToken               SpecialComment
hi link erlpegRuleRef             Function
hi link erlpegOptRuleRef          SpecialComment
hi link erlpegOptToken            Comment
hi link erlpegOptSuffix           Conditional
hi link erlpegEmpty               Macro
hi link erlpegAny                 PreProc
hi link erlpegEOF                 Include
hi link erlpegFail                Exception
hi link erlpegRanges              String
hi link erlpegRange               Character
hi link erlpegRangeDelim          Special
hi link erlpegRangeSpec1          Special
hi link erlpegRangeSpec2          Special
hi link erlpegDLitDelim           Delimiter
hi link erlpegSLitDelim           Delimiter
"hi link erlpegExecDelim           SpecialComment
hi link erlpegTaggedRule          Comment
hi link erlpegTransform           Delimiter
hi link erlpegTransMacro          Macro
hi link erlpegTransPosMacro       Include
hi link erlpegTransAtom           Constant
hi link erlpegTransDelim          Exception
hi link erlpegTransFun            Typedef
hi link erlpegCollapseFun         StorageClass
hi link erlpegCollapseFun2        StorageClass
hi link erlpegTransFun2           Typedef

let b:current_syntax = "erlpeg"

set formatoptions=tcqnl
