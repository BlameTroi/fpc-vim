" plugin/fpc.vim
" ===========================================================================
" Vim plugin file
" Language:    Pascal as fpc for Free Pascal 3.2.2
" Maintainer:  Troy Brumley <blametroi@gmail.com>
" Last Change: November 2023.
" License:     Vim License.
"
" customized fpc plugin file starting from a blank slate trying to come up
" with something i'll like for the objfpc mode of free pascal. right now this
" file does nothing of interest but it's included as a starting point if i 
" add more commands than the basic :make for fpc.
" ===========================================================================

" Only load this indent file when no other was loaded.
if exists('g:loaded_vim_fpc')
  finish
endif

let g:loaded_vim_fpc = 1

"
let s:save_cpo = &cpo
set cpo&vim

" load in a minimal DMSG if we don't see one
" txb: autoload or not, as it's global?
if !exists(":DMSG")
  echomsg "fpc-vim: DMSG not found, defining minimal."

  let g:DMSG_flag = 0

  function! g:DMSG(txt)
    if g:DMSG_flag
      echomsg a:txt
    endif
  endfunction

  command! -nargs=1 DMSG       call g:DMSG(<args>)
  command! -nargs=0 DMSGOn     let g:DMSG_flag = 1
  command! -nargs=0 DMSGOff    let g:DMSG_flag = 0
  command! -nargs=0 DMSGToggle let g:DMSG_flag = !g:DMSG_flag
endif

"
let &cpo = s:save_cpo
unlet s:save_cpo

" ============================================================================
" end plugin/fpc.vim
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
