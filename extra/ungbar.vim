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

syn match   ungbarStringModifier  /\\./ contained
syn match   ungbarStringModifier  /\~\%(-\?[0-9*]\+\)\?\%(\.[0-9*]\+\..\?\)\?\%(c\|f\|e\|g\|s\|w\|p\|W\|P\|B\|X\|#\|b\|+\|n\|i\)/ contained
syn region  ungbarString          start=+"+  skip=+\n\\\\\|\\"+  end=+"+ contains=@Spell,ungbarStringModifier
syn region  ungbarBinaryString    start=+"+  skip=+\n\\\\\|\\"+  end=+"+ contained contains=@Spell,ungbarStringModifier

syn keyword ungbarTodo            TODO FIXME XXX NOTE NOTES contained
syn match   ungbarDoc             /\(##\s*\)\@<=@\w\+/ contained
syn match   ungbarComment         /#.*$/ contains=ungbarTodo,ungbarDoc,@Spell

syn keyword ungbarBoolean         true false
syn keyword ungbarConditional     if else switch when
syn keyword ungbarUserLabel       case
syn keyword ungbarMessage         receive after
syn keyword ungbarOperator        and in not or xor orr andd
syn keyword ungbarFunction        fn begin
syn keyword ungbarException       throw try catch

syn keyword ungbarGuard           is_list is_atom is_binary
syn keyword ungbarGuard           is_bitstring is_boolean
syn keyword ungbarGuard           is_tuple is_number is_integer
syn keyword ungbarGuard           is_float is_function is_constant
syn keyword ungbarGuard           is_pid is_port is_reference
syn keyword ungbarGuard           is_record hd length size tl trunc tuple_size abs bit_size byte_size element float node round self

syn match   ungbarParamMod        /@[A-Z_]\w*/ contains=ungbarVariable
syn match   ungbarAtom            /\%(\%(^-\)\|#\)\@<!\<[a-z]\w*\>(\@!/
syn match   ungbarAtom            /\\\@<!'[^']*\\\@<!'/
syn match   ungbarVariable        /\<[A-Z_]\w*\>/
syn match   ungbarIgnoredVar      /\<_\w*\>/

syn match   ungbarModule          /\<[a-z]\w*\.\@=/ contained
syn match   ungbarFunction        /\<[a-z][A-Za-z_\.]*\s*(\@=/ contains=ungbarModule

syn match   ungbarNumber          "\<0[oO]\=\o\+[Ll]\=\>"
syn match   ungbarNumber          "\<0[xX]\x\+[Ll]\=\>"
syn match   ungbarNumber          "\<0[bB][01]\+[Ll]\=\>"
syn match   ungbarNumber          "\<\%([1-9]\d*\|0\)[Ll]\=\>"
syn match   ungbarNumber          "\<\d\+[jJ]\>"
syn match   ungbarNumber          "\<\d\+[eE][+-]\=\d\+[jJ]\=\>"
syn match   ungbarNumber          "\<\d\+\.\%([eE][+-]\=\d\+\)\=[jJ]\=\%(\W\|$\)\@="
syn match   ungbarNumber          "\%(^\|\W\)\@<=\d*\.\d\+\%([eE][+-]\=\d\+\)\=[jJ]\=\>"

syn match   ungbarSpaceError      display "\t"
syn match   ungbarSpecialChar     "'\\.'"

syn match   ungbarBitString       "<\[[^\]]\+\]>" contains=ungbarBinaryString,ungbarSep,ungbarNumber
syn match   ungbarBraces          "[{}\[\]\.,]"
syn match   ungbarParens          "[()]"
syn match   ungbarOperator        "[\$\+\-\*/\|\&~=]"
syn match   ungbarOperator        "[\+\-\*/\|\&~=]"
syn match   ungbarOperator        "[^^]\^"  "Snap- all three kinds of ^.  Cool.
syn match   ungbarSep             ":"
syn match   ungbarSep             "::"
syn match   ungbarSep             "->"
syn match   ungbarMacro           "\$[a-z_]\+"
syn match   ungbarProcessor	  "^@@\=\a\+"

syn match   ungbarVisibility      /\([<:]\|=>\)/ contained
syn match   ungbarTopFunction     /^\(<\|:\)*[a-z]\w*\s*/ contains=ungbarVisibility

syn keyword ungbarSpecial         adler32 adler32_combine apply atom_to_binary atom_to_list binary_to_atom binary_to_existing_atom binary_to_list bitstring_to_list binary_to_term check_process_code concat_binary crc32 crc32_combine date decode_packet delete_module disconnect_node erase exit float_to_list garbage_collect get get_keys group_leader halt integer_to_list iolist_to_binary iolist_size is_alive is_process_alive link list_to_atom list_to_binary list_to_bitstring list_to_existing_atom list_to_float list_to_integer list_to_pid list_to_tuple load_module make_ref monitor_node nodes now open_port pid_to_list port_close port_command port_connect port_control pre_loaded process_flag process_info processes purge_module put register registered setelement spawn spawn_link spawn_monitor spawn_opt split_binary statistics term_to_binary throw time tuple_to_list unlink unregister whereis

hi link ungbarString              String
hi link ungbarStringModifier      SpecialChar
hi link ungbarComment             Comment
hi link ungbarTodo                Todo
hi link ungbarDoc                 SpecialComment
hi link ungbarConditional         Conditional
hi link ungbarUserLabel           Tag
hi link ungbarGuard               Keyword
hi link ungbarBoolean             Boolean
hi link ungbarAtom                Normal
hi link ungbarVariable            Identifier
hi link ungbarIgnoredVar          Comment
hi link ungbarFunction            Function
hi link ungbarTopFunction         Label
hi link ungbarModule              Include
hi link ungbarVisibility          Delimiter
hi link ungbarNumber              Number
hi link ungbarSpaceError          Error
hi link ungbarSpecialChar         SpecialChar
hi link ungbarSep                 Delimiter
hi link ungbarOperator            Operator
hi link ungbarBraces              Structure
hi link ungbarParens              Type
hi link ungbarMacro               Macro
hi link ungbarProcessor           PreProc
hi link ungbarAtom                Constant
hi link ungbarException           Exception
hi link ungbarParamMod            Typedef
hi link ungbarBitString           StorageClass
hi link ungbarBinaryString        Character
hi link ungbarSpecial             Special

let b:current_syntax = "ungbar"
