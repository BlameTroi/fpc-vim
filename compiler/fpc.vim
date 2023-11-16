" compiler/fpc.vim
" ===========================================================================
" Vim compiler file
" Language:    Pascal as fpc for Free Pascal 3.2.2
" Maintainer:  Troy Brumley <blametroi@gmail.com>
" Last Change: November 2023.
" License:     Vim License.
" ===========================================================================


if exists("current_compiler")
  finish
endif


let s:savecpo = &cpo
set cpo&vim


" backfill older vims so we can use the prefered :CompilerSet
" command. see :help :CompilerSet for more information.
let current_compiler = "fpc"

if exists(":CompilerSet") != 2  " older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif


" -------------------------------------------------------------------
" compiler options are set and adjusted in file fpc.cfg which is
" part of the free pascal ecosystem. one is created during the
" install of free pascal. see their documentation for more info.
" -------------------------------------------------------------------
CompilerSet errorformat=%f(%l\\,%c)\ %m

CompilerSet makeprg=fpc\ %


let &cpo = s:savecpo
unlet s:savecpo


" ============================================================================
" end compiler/fpc.vim
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
