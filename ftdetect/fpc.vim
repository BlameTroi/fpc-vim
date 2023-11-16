" ftdetect/fpc.vim
" ===========================================================================
" Vim ftdetect plugin file
" Language:    Pascal as fpc for Free Pascal 3.2.2
" Maintainer:  Troy Brumley <blametroi@gmail.com>
" Last Change: November 2023.
" License:     Vim License.
"
" neovim changes things up so the traditional autocmnd wasn't working the
" way i expected. looking at filetype.lua and some issues for neovim on
" github, i think this is the best portable solution. it has the virtua of
" working on my systems.
" ===========================================================================


if has('nvim')
  lua vim.filetype.add({ extension = {pp = 'fpc', p = 'fpc', pas = 'fpc',}, })
else
  autocmd BufNewFile,BufRead *.pas,*.pp,*.p set filetype=fpc
endif


" ============================================================================
" end ftdetect/fpc.vim
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
