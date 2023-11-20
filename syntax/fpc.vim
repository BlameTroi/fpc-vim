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


if exists('b:current_syntax')
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


" reserved words that influence code structure (both logical and
" in terms of visual layout) are given fine grained highlight
" names to help with indenting and other functins of the plugin.
syntax keyword fpcBegin           begin
syntax keyword fpcCase            case
syntax keyword fpcConst           const
syntax keyword fpcDo              do
syntax keyword fpcElse            else
syntax keyword fpcEnd             end
syntax keyword fpcFinalization    finalization
syntax keyword fpcFor             for
syntax keyword fpcFunction        function
syntax keyword fpcGoto            goto
syntax keyword fpcIf              if
syntax keyword fpcImplementation  implementation
syntax keyword fpcInitialization  initialization
syntax keyword fpcInterface       interface
syntax keyword fpcLabel           label
syntax keyword fpcProcedure       procedure
syntax keyword fpcProgram         program
syntax keyword fpcRepeat          repeat
syntax keyword fpcThen            then
syntax keyword fpcType            type
syntax keyword fpcUnit            unit
syntax keyword fpcUntil           until
syntax keyword fpcVar             var
syntax keyword fpcWhile           while
syntax keyword fpcWith            with

" the following reserved words don't have any meaningful
" impact on code structure but do need to be highlighted.
" some grouping on broad classification but not as fine
" grained as above.
"
" note that most of these have plural names.
syntax keyword fpcWords array downto file of packed record
syntax keyword fpcWords set to
syntax keyword fpcDirectives forward
syntax keyword fpcConstants true false maxint nil
syntax keyword fpcTypes boolean char integer real


" standard required functions and procedures
syntax keyword fpcFunctions abs arctan cos exp ln sin sqr sqrt 
syntax keyword fpcFunctions trunc round
syntax keyword fpcFunctions chr ord pred succ
syntax keyword fpcFunctions odd eof eoln
syntax keyword fpcFunctions new dispose


" standard required input output
syntax keyword fpcIoFunctions read readln reset rewrite write writeln
syntax keyword fpcIoFunctions get put page


" obvious non-optional extensions
syntax keyword fpcExtDirectives class private public protected published
syntax keyword fpcExtDirectives property static abstract virual dynamic
syntax keyword fpcExtDirectives override overload
syntax keyword fpcExtTypes string ansistring
syntax keyword fpcExtTypes shortint longint
syntax keyword fpcExtFunctions assign close length
syntax keyword fpcExtFunctions high low setlength
syntax keyword fpcExtWords break continue exit result


" exception handling
" txb: these will probably need to be individually defined for indenting
syntax keyword fpcExtExcept try catch finally except raise exception


" link to the highlight groups
highlight default link fpcComment Comment
highlight default link fpcConstants Constant
highlight default link fpcDirectives Keyword
highlight default link fpcFunctions Function
highlight default link fpcIoFunctions Function
highlight default link fpcIdentifier Identifier
highlight default link fpcInteger Number
highlight default link fpcOperator Operator
highlight default link fpcReal Number
highlight default link fpcString String
highlight default link fpcTypes Type
highlight default link fpcWords Keyword
highlight default link fpcDelimiters Delimiter


" these are more fine grained for use in synIDattr suppported
" search skips.
highlight default link fpcBegin           keyword
highlight default link fpcCase            keyword
highlight default link fpcConst           keyword
highlight default link fpcDo              keyword
highlight default link fpcElse            keyword
highlight default link fpcEnd             keyword
highlight default link fpcFinalization    keyword
highlight default link fpcFor             keyword
highlight default link fpcFunction        keyword
highlight default link fpcGoto            keyword
highlight default link fpcIf              keyword
highlight default link fpcImplementation  keyword
highlight default link fpcInitialization  keyword
highlight default link fpcInterface       keyword
highlight default link fpcLabel           keyword
highlight default link fpcProcedure       keyword
highlight default link fpcProgram         keyword
highlight default link fpcRepeat          keyword
highlight default link fpcThen            keyword
highlight default link fpcType            keyword
highlight default link fpcUnit            keyword
highlight default link fpcUntil           keyword
highlight default link fpcVar             keyword
highlight default link fpcWhile           keyword
highlight default link fpcWith            keyword
highlight default link fpcComment         keyword

" extensions, currently they get the same highlighting 
" as the standard or base items
highlight default link fpcExtConstants Constant
highlight default link fpcExtDirectives Keyword
highlight default link fpcExtExcept Keyword
highlight default link fpcExtFunctions Function
highlight default link fpcExtTypes Type
highlight default link fpcExtWords Keyword


" candy
highlight default link fpcTodo Todo


" and that's a wrap
let b:current_syntax = 'fpc'

let &cpo = s:save_cpo
unlet s:save_cpo


" ============================================================================
" end syntax/fpc.vim
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
