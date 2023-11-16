" ftplugin/fpc.vim
" ===========================================================================
" Vim filetype plugin file
" Language:    Pascal as fpc for Free Pascal 3.2.2
" Maintainer:  Troy Brumley <blametroi@gmail.com>
" Last Change: November 2023.
" License:     Vim License.
" ===========================================================================

" do this only once
if exists("b:did_ftplugin")
	finish
endif
let b:did_ftplugin = 1


" -------------------------------------------------------------------
" good plugin hygene
" -------------------------------------------------------------------
let s:save_cpo = &cpo
set cpo&vim


" -------------------------------------------------------------------
" this plugin expects expandtabs and doesn't really support folding.
"
" while in insert mode just copy whatever indent is on the prior line,
" allowing the user to adjust as they see fit. a full indent via =
" will realign stuff.
"
" <c-f> while in insert mode will invoke the indent plugin.
" -------------------------------------------------------------------
setlocal expandtab 
setlocal foldmethod=manual
setlocal autoindent
setlocal copyindent
setlocal indentkeys=!^F


" -------------------------------------------------------------------
" attempt to use comment entry and formatting support.
" txb: need to set up undo for comments, formatoptions
" -------------------------------------------------------------------
set comments=s:(*,m:\ ,e:*),s:{,m:\ ,e:}
set commentstring={%s}

" set commentstring=//\ %s
" set comments+=://

" no autowrap at textwidth for code
" but otherwise sensible for text in comments
setlocal formatoptions-=t formatoptions+=jcroql1n


" -------------------------------------------------------------------
" build ongoing undo information for setting changes.
" -------------------------------------------------------------------
if exists('b:undo_ftplugin') && !empty(b:undo_ftplugin)
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif
let b:undo_ftplugin .= "setlocal noexpandtab< foldmethod< copyindent< autoindent< indentkeys< formatoptions<" 


" -------------------------------------------------------------------
" matchit support
"
" match words are a comma delimited list of begin:end patterns. put
" these in b:match_words and the matchit plugin will jump back and
" forth between the begining and ending words when % is pressed in
" normal mode.
"
" i don't find this particularly helpful and am only matching
" bracketing words. constructs such as while-do and for-to just don't
" need the jumps and if you can't see the then that matches an if, or
" spot the various case selections, you've got problems beyond
" needing something like matchit.
" -------------------------------------------------------------------
if exists("loaded_matchit")
  " case is not significant in pascal
  let b:match_ignorecase = 1
	" startline pattern
	let s:sl = '^\s*'
  let b:match_words = ''
  let b:match_words .= '\<\%(begin\|record\|object\)\>:\<end\>'
  let b:match_words .= ','.s:sl.'\<unit\>'.':'.s:sl.'\<interface\>'.':'.s:sl.'\<implementation\>'.':'.s:sl.'\<initialization\>'.':'.s:sl.'\<finalization\>'.':'.s:sl.'\<end\.'
  let b:match_words .= ','.'\<repeat\>:\<until\>'
endif


" -------------------------------------------------------------------
" build ongoing undo information for setting changes.
" -------------------------------------------------------------------
if exists('b:undo_ftplugin') && !empty(b:undo_ftplugin)
  let b:undo_ftplugin .= ' | '
else
  let b:undo_ftplugin = ''
endif
let b:undo_ftplugin .= "unlet! b:match_words"


" -------------------------------------------------------------------
" put cpoptions back
" -------------------------------------------------------------------
let &cpo = s:save_cpo
unlet s:save_cpo


" ============================================================================
" end ftplugin/fpc.vim
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
