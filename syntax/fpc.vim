" syntax/fpc.vim
" ===========================================================================
" Vim filetype plugin file
" Language:    Pascal as fpc for Free Pascal 3.2.2
" Maintainer:  Troy Brumley <blametroi@gmail.com>
" Last Change: November 2023.
" License:     Vim License.
"
" customized fpc syntax file starting from a blank slate trying to come up
" with a simple syntax that works for the objfpc mode of free pascal. the
" highlighting in the built in syntax is pretty good but i don't want the
" various words from modes or compilers i don't use to be highlighted by
" default.
"
" i'm also planning to reduce the number of highlight colors.
" ===========================================================================


if exists("b:current_syntax")
  finish
endif

let s:save_cpo = &cpo
set cpo&vim


" pascal is case insensitive
syntax case ignore

" is there a better setting? 
syntax sync fromstart ccomment fpcComment 

" numbers
syntax match fpcInteger "-\=\<\d\+\>"
syntax match fpcReal "-\=\<\d\+\.\d\+\>"
syntax match fpcReal "-\=\<\d\+\.\d\+[e|E][+|-]\=\d\+\>"
syntax match fpcReal "-\=\<\d\+[eE][+-]\=\d\+\>"


" special characters as operators or delimiters and
" the keyword operators. delimiter : needs to be
" defined in front of operator := to avoid hilighting
" := in two colors.
syntax match fpcDelimiter "[\,\;\:\(\)]"
syntax match fpcOperator "[+\-/*=]"
syntax match fpcOperator ":="
syntax match fpcOperator "<>"
syntax match fpcOperator "<="
syntax match fpcOperator "<"
syntax match fpcOperator ">="
syntax match fpcOperator "\.\."
syntax match fpcOperator "\^"
syntax match fpcOperator "\."
syntax match fpcOperator "[][]"
syntax match fpcOperator "\v/"
syntax match fpcOperator "\v\="
syntax match fpcOperator "\.\."
syntax keyword fpcOperator and div in mod not or shl shr xor


" comments ... i've seen line comments defined as a match, but
" since it extends to end-of-line i decided to try region and
" it works fine.
syntax region fpcComment start="(\*\|{"  end="\*)\|}"
syntax region fpcComment start="\v//" end="$"


" highlight todoish items in comments. i got bored and looked
" through all the syntax files in the base runtime and found
" that the most frequent highlighted words are todo, fixme,
" bug, and xxx.
syntax match fpcTodo "\<bug\:" contained containedin=fpcComment
syntax match fpcTodo "\<fixme\:" contained containedin=fpcComment
syntax match fpcTodo "\<todo\:" contained containedin=fpcComment
syntax match fpcTodo "\<xxx\:" contained containedin=fpcComment


" strings
syntax region fpcString matchgroup=fpcString start=+'+ end=+'+ contains=fpcStringEscape
syntax match fpcStringEscape	contained "''"


" i'm unsure of the value of this over just default, but let's try it
syntax match fpcIdentifier "\<[a-zA-Z_][a-zA-Z0-9_]*\>"


" grouping based on the standard and then layering extensions on top
" highlighting by function leads to gratuitous colors
syntax keyword fpcWord array begin case const do downto
syntax keyword fpcWord else end file for function goto if
syntax keyword fpcWord label of packed procedure program
syntax keyword fpcWord record repeat set then to type
syntax keyword fpcWord until var while with
syntax keyword fpcDirective forward
syntax keyword fpcConstant true false maxint nil
syntax keyword fpcType boolean char integer real


" standard required functions and procedures
syntax keyword fpcFunction abs arctan cos exp ln sin sqr sqrt 
syntax keyword fpcFunction trunc round
syntax keyword fpcFunction chr ord pred succ
syntax keyword fpcFunction odd eof eoln
syntax keyword fpcFunction new dispose


" standard required input output
syntax keyword fpcIO read readln reset rewrite write writeln
syntax keyword fpcIO get put page


" obvious non-optional extensions
syntax keyword fpcExtDirective finalization implementation
syntax keyword fpcExtDirective initialization interface unit uses
syntax keyword fpcExtDirective class private public protected published
syntax keyword fpcExtDirective property static abstract virual dynamic
syntax keyword fpcExtDirective override overload
syntax keyword fpcExtType string ansistring
syntax keyword fpcExtType shortint longint
syntax keyword fpcExtFunction assign close length
syntax keyword fpcExtFunction high low setlength
syntax keyword fpcExtWord break continue exit result


" exception handling
syntax keyword fpcExtExcept try catch finally except raise exception


" link to the highlight groups
highlight default link fpcComment Comment
highlight default link fpcConstant Constant
highlight default link fpcDirective Keyword
highlight default link fpcFunction Function
highlight default link fpcIO Function
highlight default link fpcIdentifier Identifier
highlight default link fpcInteger Number
highlight default link fpcOperator Operator
highlight default link fpcReal Number
highlight default link fpcString String
highlight default link fpcType Type
highlight default link fpcWord Keyword
highlight default link fpcDelimiter Delimiter


" extensions, currently they get the same as the standard or base
" items
highlight default link fpcExtConstant Constant
highlight default link fpcExtDirective Keyword
highlight default link fpcExtExcept Keyword
highlight default link fpcExtFunction Function
highlight default link fpcExtType Type
highlight default link fpcExtWord Keyword


" candy
highlight default link fpcTodo Todo


" and that's a wrap
let b:current_syntax = "fpc"

let &cpo = s:save_cpo
unlet s:save_cpo


" ============================================================================
" end syntax/fpc.vim
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
