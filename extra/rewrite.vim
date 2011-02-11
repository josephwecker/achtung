if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" TODO: Move somewhere else at some point ----------
set ambiwidth=double
" epsilon: Empty string / success
map! \eps ɛ
map! \succ ɛ
map! \empty ɛ
" digamma: failure / !ɛ
map! \fail ϝ
map! \dig ϝ
map! := ≔
map! \|= ⊨
map! <- ←
map! -> →
map! \\ ⑊
map! << ⦑
map! >> ⦒
"map! <<< 《
"map! << 〈
"map! >>> 》
"map! >> 〉
map! ++ ⧺

" x -= y  (means)  x in y:list
" x =- y  (means)  y:list having-member x
map! -= ∈
map! =- ∋

" --------------------------------------------------
syn include @AsciiDoc syntax/asciidoc.vim

syn match   rewriteStringModifier  /\\./ contained
syn match   rewriteStringModifier  /\~\%(-\?[0-9*]\+\)\?\%(\.[0-9*]\+\..\?\)\?\%(c\|f\|e\|g\|s\|w\|p\|W\|P\|B\|X\|#\|b\|+\|n\|i\)/ contained
syn region  rewriteString          start=+"+  skip=+\n\\\\\|\\"+  end=+"+ contains=@Spell,rewriteStringModifier
syn region  rewriteBinaryString    start=+"+  skip=+\n\\\\\|\\"+  end=+"+ contained contains=@Spell,rewriteStringModifier

syn keyword rewriteTodo            TODO FIXME XXX NOTE NOTES contained
syn match   rewriteDoc             /\(##\s*\)\@<=@[A-Za-z_]\+/ contained
syn match   rewriteComment         /#.*$/ contains=rewriteTodo,rewriteDoc,@Spell
syn region  rewriteBlockComment    start="#|\s\+" skip="\(\\#|\|\\|#\)" end="|#" contains=rewriteBlockComment,rewriteTodo,rewriteDoc,@Spell
syn region  rewriteBlockAsciiComment start="#|asciidoc|" skip="\(\\#|\|\\|#\)" end="|asciidoc|#" contains=@Spell,@AsciiDoc,rewriteACDelim keepend
syn match   rewriteACDelim         /#|asciidoc|\||asciidoc|#/ contained

syn match   rewriteAtom            /\%(\%(^-\)\|#\)\@<!\<[a-z][A-Za-z0-9_-]*\>(\@!/
syn match   rewriteAtom            /\\\@<!'[^']*\\\@<!'/
syn match   rewriteVariable        /[A-Z_@⌀][A-Za-z0-9_@-]*\>/
syn match   rewriteIgnoredVar      /\<_[A-Za-z0-9@_-]*\>/

syn match   rewriteAliasName       /[A-Z_@⌀-][A-Za-z0-9_@-]*\>\s*\(:=\|≔\)/ contains=rewriteOperators

syn match   rewriteListBrackets    /\[\|\]/
syn match   rewriteTupleBrackets   /(\|)/
syn region  rewriteMapClause       start=/|/ end=/|=\|⊨/ contains=TOP
syn match   rewriteOperators       /:=\|≔\|⦒\|>>\|⦑\|<<\|∋\|-=\|∈\|=-/
syn match   rewriteDelimiters      /\//

syn match   rewriteRuleName        /^[a-z][A-Za-z0-9_\/-]*\s*/ contains=rewriteDelimiters

hi link rewriteRuleName            Label
hi link rewriteMapClause           Conditional
hi link rewriteListBrackets        Type
hi link rewriteTupleBrackets       Structure
hi link rewriteStringModifier      SpecialChar
hi link rewriteString              String
hi link rewriteTodo                SpecialComment
hi link rewriteDoc                 SpecialComment
hi link rewriteComment             Comment
hi link rewriteBlockComment        Comment
hi link rewriteACDelim             Comment
hi link rewriteAtom                Constant
hi link rewriteVariable            Identifier
hi link rewriteIgnoredVar          Comment
hi link rewriteAliasName           Macro
hi link rewriteOperators           Operator
hi link rewriteDelimiters          Delimiter

let b:current_syntax = "rewrite"
