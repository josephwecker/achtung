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

map! |= ⊧
map! <- ←
map! -> →
map! \\ ⑊
map! <<< 《
map! << 〈
map! >>> 》
map! >> 〉
" --------------------------------------------------

let b:current_syntax = "rewrite"
