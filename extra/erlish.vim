" TODO:
"   - Bitstrings
"   - Keywords
"   - Constants
"   - Records
"   - Structs
"   - PreProcessors (ASTify etc.)
"   - Special module stuff like @doc etc.
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match   erlishStringModifier  /\\./ contained
syn match   erlishStringModifier  /\~\%(-\?[0-9*]\+\)\?\%(\.[0-9*]\+\..\?\)\?\%(c\|f\|e\|g\|s\|w\|p\|W\|P\|B\|X\|#\|b\|+\|n\|i\)/ contained
syn region  erlishString          start=+"+  skip=+\n\\\\\|\\"+  end=+"+ contains=@Spell,erlishStringModifier
syn region  erlishBinaryString    start=+"+  skip=+\n\\\\\|\\"+  end=+"+ contained contains=@Spell,erlishStringModifier

syn keyword erlishTodo            TODO FIXME XXX NOTE NOTES contained
syn match   erlishDoc             /\(##\s*\)\@<=@\w\+/ contained
syn match   erlishComment         /#.*$/ contains=erlishTodo,erlishDoc,@Spell

syn keyword erlishBoolean         true false
syn keyword erlishConditional     if else switch when
syn keyword erlishUserLabel       case
syn keyword erlishMessage         receive after
syn keyword erlishOperator        and in not or xor orr andd
syn keyword erlishFunction        fn begin
syn keyword erlishException       throw try catch

syn keyword erlishGuard           is_list is_atom is_binary
syn keyword erlishGuard           is_bitstring is_boolean
syn keyword erlishGuard           is_tuple is_number is_integer
syn keyword erlishGuard           is_float is_function is_constant
syn keyword erlishGuard           is_pid is_port is_reference
syn keyword erlishGuard           is_record hd length size tl trunc tuple_size abs bit_size byte_size element float node round self

syn match   erlishParamMod        /@[A-Z_]\w*/ contains=erlishVariable
syn match   erlishAtom            /\%(\%(^-\)\|#\)\@<!\<[a-z]\w*\>(\@!/
syn match   erlishAtom            /\\\@<!'[^']*\\\@<!'/
syn match   erlishVariable        /\<[A-Z_]\w*\>/
syn match   erlishIgnoredVar      /\<_\w*\>/

syn match   erlishModule          /\<[a-z]\w*\.\@=/ contained
syn match   erlishFunction        /\<[a-z][A-Za-z_\.]*\s*(\@=/ contains=erlishModule

syn match   erlishNumber          "\<0[oO]\=\o\+[Ll]\=\>"
syn match   erlishNumber          "\<0[xX]\x\+[Ll]\=\>"
syn match   erlishNumber          "\<0[bB][01]\+[Ll]\=\>"
syn match   erlishNumber          "\<\%([1-9]\d*\|0\)[Ll]\=\>"
syn match   erlishNumber          "\<\d\+[jJ]\>"
syn match   erlishNumber          "\<\d\+[eE][+-]\=\d\+[jJ]\=\>"
syn match   erlishNumber          "\<\d\+\.\%([eE][+-]\=\d\+\)\=[jJ]\=\%(\W\|$\)\@="
syn match   erlishNumber          "\%(^\|\W\)\@<=\d*\.\d\+\%([eE][+-]\=\d\+\)\=[jJ]\=\>"

syn match   erlishSpaceError      display "\t"
syn match   erlishSpecialChar     "'\\.'"

syn match   erlishBitString       "<\[[^\]]\+\]>" contains=erlishBinaryString,erlishSep,erlishNumber
syn match   erlishBraces          "[{}\[\]\.,]"
syn match   erlishParens          "[()]"
syn match   erlishOperator        "[\$\+\-\*/\|\&~=]"
syn match   erlishOperator        "[\+\-\*/\|\&~=]"
syn match   erlishOperator        "[^^]\^"  "Snap- all three kinds of ^.  Cool.
syn match   erlishSep             ":"
syn match   erlishSep             "::"
syn match   erlishSep             "->"
syn match   erlishMacro           "\$[a-z_]\+"
syn match   erlishProcessor	  "^@@\=\a\+"

syn match   erlishVisibility      /[<:]/ contained
syn match   erlishTopFunction     /^\(<\|:\)*[a-z]\w*\s*/ contains=erlishVisibility

syn keyword erlishSpecial         adler32 adler32_combine apply atom_to_binary atom_to_list binary_to_atom binary_to_existing_atom binary_to_list bitstring_to_list binary_to_term check_process_code concat_binary crc32 crc32_combine date decode_packet delete_module disconnect_node erase exit float_to_list garbage_collect get get_keys group_leader halt integer_to_list iolist_to_binary iolist_size is_alive is_process_alive link list_to_atom list_to_binary list_to_bitstring list_to_existing_atom list_to_float list_to_integer list_to_pid list_to_tuple load_module make_ref monitor_node nodes now open_port pid_to_list port_close port_command port_connect port_control pre_loaded process_flag process_info processes purge_module put register registered setelement spawn spawn_link spawn_monitor spawn_opt split_binary statistics term_to_binary throw time tuple_to_list unlink unregister whereis

hi link erlishString              String
hi link erlishStringModifier      SpecialChar
hi link erlishComment             Comment
hi link erlishTodo                Todo
hi link erlishDoc                 SpecialComment
hi link erlishConditional         Conditional
hi link erlishUserLabel           Tag
hi link erlishGuard               Keyword
hi link erlishBoolean             Boolean
hi link erlishAtom                Normal
hi link erlishVariable            Identifier
hi link erlishIgnoredVar          Comment
hi link erlishFunction            Function
hi link erlishTopFunction         Label
hi link erlishModule              Include
hi link erlishVisibility          Delimiter
hi link erlishNumber              Number
hi link erlishSpaceError          Error
hi link erlishSpecialChar         SpecialChar
hi link erlishSep                 Delimiter
hi link erlishOperator            Operator
hi link erlishBraces              Structure
hi link erlishParens              Type
hi link erlishMacro               Macro
hi link erlishProcessor           PreProc
hi link erlishAtom                Constant
hi link erlishException           Exception
hi link erlishParamMod            Typedef
hi link erlishBitString           StorageClass
hi link erlishBinaryString        Character
hi link erlishSpecial             Special

let b:current_syntax = "erlish"
