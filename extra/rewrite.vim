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

map! \|= ⊧
map! <- ←
map! -> →
map! \\ ⑊
map! <<< 《
map! << 〈
map! >>> 》
map! >> 〉

" --------------------------------------------------
syn match   rewriteStringModifier  /\\./ contained
syn match   rewriteStringModifier  /\~\%(-\?[0-9*]\+\)\?\%(\.[0-9*]\+\..\?\)\?\%(c\|f\|e\|g\|s\|w\|p\|W\|P\|B\|X\|#\|b\|+\|n\|i\)/ contained
syn region  rewriteString          start=+"+  skip=+\n\\\\\|\\"+  end=+"+ contains=@Spell,rewriteStringModifier
syn region  rewriteBinaryString    start=+"+  skip=+\n\\\\\|\\"+  end=+"+ contained contains=@Spell,rewriteStringModifier

syn keyword rewriteTodo            TODO FIXME XXX NOTE NOTES contained
syn match   rewriteDoc             /\(##\s*\)\@<=@[A-Za-z_]\+/ contained
syn match   rewriteComment         /#.*$/ contains=rewriteTodo,rewriteDoc,@Spell
syn region  rewriteBlockComment    start="#|" skip="\(\\#|\|\\|#\)" end="|#" contains=rewriteBlockComment,rewriteTodo,rewriteDoc,@Spell

syn match   rewriteAtom            /\%(\%(^-\)\|#\)\@<!\<[a-z][A-Za-z0-9_]*\>(\@!/
syn match   rewriteAtom            /\\\@<!'[^']*\\\@<!'/
syn match   rewriteVariable        /[A-Z_@][A-Za-z0-9_@]*\>/
syn match   rewriteIgnoredVar      /\<_[A-Za-z0-9@_]*\>/

hi link rewriteStringModifier      SpecialChar
hi link rewriteString              String
hi link rewriteTodo                SpecialComment
hi link rewriteDoc                 SpecialComment
hi link rewriteComment             Comment
hi link rewriteBlockComment        Comment

hi link rewriteAtom                Constant
hi link rewriteVariable            Identifier
hi link rewriteIgnoredVar          Comment

let b:current_syntax = "rewrite"
