" indent/fpc.vim
" ===================================================================
" Vim indent file
" Language:    Pascal as fpc for Free Pascal 3.2.2
" Maintainer:  Troy Brumley <blametroi@gmail.com>
" Last Change: November 2023.
" License:     Vim License.
"
" customized fpc indent file starting from a blank slate trying to
" come up with something i'll like for the objfpc mode of free
" pascal. existing indenting and formatting for pascal and delphi
" don't do what i want.
"
" hoping for simple implementation of simple rules and clean looking
" pascal.
" ===================================================================


" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1


let s:save_cpo = &cpo
set cpo&vim


" ------------------------------------------------------------------
" this probably belongs in ftplugin/fpc.vim, but i'm not dealing
" with cross file scoping with <PLUG> or autoload yet. when i did
" have this in ftplugin, BufReadPost could also be used, but that
" doesn't work from indent.vim.
"
" TextChanged fires at the end of a filter pass (gg=G) which is
" a shame for my use, but i don't see a way to safely not do the
" cache reset.
"
" txb: still trying to decide if this should be <buffer> or file
"      pattern. 
" ------------------------------------------------------------------
".augroup fpc_vim_indent
".  autocmd!
".  autocmd TextChanged,InsertLeave <buffer> call g:FpcResetAnalysis()
"augroup END

" beyond the b:did and b:undo standards for plugins, these are
" buffer local variables that are global to indenting. hoping
" to remove almost all of these.

" ------------------------------------------------------------------
let b:indenting = 1
let b:backtrack_boundary = 0
let b:comment_start_line = 0
let b:comment_end_line = 0
let b:in_comment = 0
" ------------------------------------------------------------------

" txb: set to 1 to enable DMSG throughout
"      or do selectively in functions around
"      trouble spots. DMSGOn and DMSGOff work
"      there too.
let g:DMSG_flag = 0

".function! g:FpcResetAnalysis()
".  let b:indenting = 1
".  let b:backtrack_boundary = 0
".  let b:comment_start_line = 0
".  let b:comment_end_line = 0
".  let b:in_comment = 0
".endfunction


" ------------------------------------------------------------------
" indent doesn't fire without an expression.
"
" while indentkeys aren't being used at present, both indentkeys and
" = (filtering) will use indentexpr.
"
" note the passing of v:lnum instead of using the global directly in
" FpcGetIndent(). this allows calling from command mode or from
" other functions.
" ------------------------------------------------------------------
setlocal indentexpr=g:FpcGetIndent(v:lnum)

function! g:FpcGetIndent(of_line)

  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line

  " first line or invalid line always indent 0
  if curr_num < 2 || cur_num > line('$')
    return 0
  endif

  let curr_line = getline(curr_num)

  " let g:DMSG_flag = b:in_comment || (curr_num >= 35 && curr_num <= 38)

  " format/indent directives {##indent:on|off} can turn off
  " indent processing for a block of lines. defaults to on.
  if curr_line =~ '\v\c^\s*((\{\#\#|\(\*\#\#))indent'
    let b:indenting = -1
    if curr_line =~ '\v\c<indent[: =-]off>'
      let b:indenting = 0
    endif
    if curr_line =~ '\v\c<indnet[: =-]on>'
      let b:indenting = 1
    endif
    if b:indenting == -1
      echoerr printf("fpc-indent: bad ##indent directive ignored at line: %d, '%s'", curr_num, matchstr(curr_line, '\v\c((\{\#\#)|(\(\*\#\#)).*'))
      let b:indenting = 1
    endif
  endif

  " preprocessor commands reet indent regardless of indent setting
  if g:FpcIsPreprocessing(curr_num)
    DMSG printf("fpc-indent: preprocessor directive at line: %d '%s'", curr_num, curr_line)
    return indent(curr_num) == 0 ? -1 : 0
  endif
    
  " preprocessor directives have their indent set to 0, while blank
  " and comment lines are unchanged.

  " check was failing when open was not closed after code, as in
  " 'do begin { ...'
  DMSG printf("fpc-indent: past !indenting check at line %d", curr_num)
  if curr_line =~ '\v^\.*(\{|\(\*)' && curr_line !~ '\v(\}|\*\))'
    if b:in_coment
      echomsg "ERROR, already in comment at " .. curr_num .. ":" curr_line
    endif
    DMSG printf("fpc-indent: start comment block at line: %d, '%s'", curr_num, curr_line)
    let b:comment_start_line = curr_num
    let b:comment_end_line = 0
    let b:in_comment = 1
    return -1
  endif
  if !b:in_coment && curr_line =~ '\v(\}|\*\))'
      echomsg "ERROR, already in comment at " .. curr_num .. ":" curr_line
    endif
  if b:in_comment && curr_line =~ '\v(\}|\*\))'
    DMSG printf("fpc-indent: end comment block at line: %d, '%s'", curr_num, curr_line)
    let b:comment_end_line = curr_num
    let b:in_comment = 0
    return -1
  endif
  if b:in_comment
    DMSG printf("fpc-indent: in comment at: %d", curr_num)
    return -1
  endif

  " comment lines, blank lines, and anything when not indenting
  " are all unchanged
  if !b:indenting || g:FpcIsBlankOrLeadingComment(curr_num)
    return -1
  endif

  " this line may be indentable
  let curr_indent = indent(curr_num)

  " these lines all have hard left justification and are boundary
  " lines for look behinds. the ternary expression return is a
  " possibly unneeded optimization, but just maybe vim or neovim
  " won't flag textchanged for these lines.
  if g:FpcIsBoundaryLine(curr_num)
    let b:backtrack_boundary = curr_num
    return curr_indent == 0 ? -1 : 0
  endif

  " sanitize the line by removing strings and comments. i might
  " be able to combine into one routine but at one point the
  " split seemed necessary. this sanitizing does not alter the
  " actual line in the buffer.
  let curr_line = g:FpcStripStrings(curr_line)
  let curr_line = g:FpcStripComments(curr_line)

  " generally take prior indent unless that line indents us but
  " a leading begin is a special case. also not that while the
  " check here is for begin at start of line, the pattern sent
  " for pair searching is for begin anywhere.
  if curr_line =~ '\v\c^\s*begin>'
    let g:DMSG_flag = 1
    DMSG printf("fpc-indent: leading/only begin seeking correct prior line for indent from line: %d, '%s'", curr_num, curr_line)
    let prev_num = g:FpcGetPriorPair(curr_num,
          \ '\v\c<(program|procedure|function|var|type|const|label|if|then|else|while|do|with)>',
          \ '\v\c<begin>')
    let prev_line = getline(prev_num)
    let prev_indent = indent(prev_num)
    let g:DMSG_flag = 0
    return curr_indent == prev_indent ? -1 : prev_indent
  endif

  " handle the closing of a compound statement. these can
  " probably be folded together.
  "   end -- begin/case/record
  "   until -- repeat
  if curr_line =~ '\v\c<end>'
    DMSG printf("fpc-indent: end seeking matching begin-case-record from line: %d '%s'", curr_num, curr_line)
    let prev_num = g:FpcGetPriorPair(curr_num, '\v\c<(begin|record|case)>', '\v\c<end>')
    let prev_line = getline(prev_num)
    let prev_indent = indent(prev_num)
    return curr_indent == prev_indent ? -1 : prev_indent
  endif

  if curr_line =~ '\v\c<until>'
    DMSG printf("fpc-indent: until seeking matching begin-case-with/repeat from line: %d '%s'", curr_num, curr_line)
    let prev_num = g:FpcGetPriorPair(curr_num, '\v\c<repeat>', '\v\c<until>')
    let prev_line = getline(prev_num)
    let prev_indent = indent(prev_num)
    return curr_indent == prev_indent ? -1 : prev_indent
  endif

  " look back to the prior code line to get the indent for the
  " current line. some of those code lines indent this line.
  " including the various parts of some statements (then and
  " else of if, for example) along with begin and end should
  " properly handle the many arrangements of things such as
  " if-then-begin-end-else ...
  let prev_num = g:FpcGetPrior(curr_num)
  let prev_line = getline(prev_num)
  let prev_indent = indent(prev_num)
  if prev_line =~ '\v\c<(begin|label|const|type|var|if|for|while|with|repeat|case|record|do|then|else)>'
    return curr_indent == prev_indent + &shiftwidth ? -1 : prev_indent + &shiftwidth
  endif
  return curr_indent == prev_indent ? -1 : prev_indent

endfunction

" -------------------------------------------------------------------
" find backtrack hard boundary line, does *not* reset
" b:backtrack_boundary. this search currently doesn't handle
" comments or indent supression.
" -------------------------------------------------------------------

  let s:match_boundary_line = '\v\c^\s*((procedure|function|label|var|program|unit|uses|const|type|interface|implementation|initialization|finalization)>|end\.)'

function! g:FpcGetBacktrackBoundary(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  if curr_num == 0
    return 0
  endif
  let save_cursor = getcurpos()
  call cursor(curr_num, 1)
  let prior_num = search(s:match_boundary_line, "bW")
  if prior_num == 0
    DMSG printf("fpc-indent: could not find boundary for line: %d", curr_num)
  else
   DMSG printf("fpc-indent: found boundary for line: %d at line %d", curr_num, prior_num)
  endif
  call cursor(save_cursor[1], save_cursor[2])
  return prior_num
endfunction


" -------------------------------------------------------------------
" many functions want line numbers but could be passed marks. for
" testing during development, passing raw text (eg, to FpcIs...)
" is worthwhile. this is a dwim helper. marks ., $, <, >, and a-z
" are supported.
" -------------------------------------------------------------------
" txb: wire this in
function! g:FpcTextOrNumberFrom(arg)
  if type(a:arg) == v:t_number
    return a:arg
  endif
  if type(a:arg) != v:t_string
    return 0
  endif
  return a:arg =~ '\v^(\.|\$)|(\''[a-z<>])'
        \ ? line(a:arg)
        \ : a:arg
endfunction
 

" -------------------------------------------------------------------
" a boundary line is one that ends a backward search. 
" -------------------------------------------------------------------
function! g:FpcIsBoundaryLine(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  return curr_num == 0
        \ ? 1
        \ : getline(curr_num) =~ s:match_boundary_line
endfunction


" -------------------------------------------------------------------
" line is blank or has a leading comment
" -------------------------------------------------------------------
function! g:FpcIsBlankOrLeadingComment(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  let curr_line = getline(curr_num)
  return curr_line =~ '\v\c^\s*$' ||
        \ curr_line =~ '\v\c^\s*((\{.*\})|(\(\*.*\*\))|\/\/)\.*$'
endfunction


" -------------------------------------------------------------------
" is a line a preprocesosr directive? {$...} (*$...*) {##...} #
" -------------------------------------------------------------------
function! g:FpcIsPreprocessing(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  return curr_num == 0
        \ ? 0
        \ : getline(curr_num) =~ '\v\c^\s*(((\{|\(\*)(\$|\#\#))|\#)'
endfunction


" -------------------------------------------------------------------
" find prior code line 
" -------------------------------------------------------------------
function! g:FpcGetPrior(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line

  if curr_num == 0
    return 0
  endif

  DMSG printf("fpc-indent: seeking prior for line at: %d, boundary at: %d, comment range: %d - %d", curr_num, b:backtrack_boundary, b:comment_start_line, b:comment_end_line)

  let stop_search = g:FpcGetBacktrackBoundary(curr_num)
  while curr_num > stop_search
    let curr_num = prevnonblank(curr_num - 1)
    DMSG printf("checking %d", curr_num)
    if b:comment_start_line <= curr_num && curr_num <= b:comment_end_line
      DMSG printf("fpc-indent: line %d is in comment range %d - %d", curr_num, b:comment_start_line, b:comment_end_line)
      continue
    endif
    if g:FpcIsBlankOrLeadingComment(curr_num)
      DMSG printf("fpc-indent: line %d blank or leading comment '%s'", curr_num, curr_line)
      continue
    endif
    break
  endwhile

  DMSG printf("fpc-indent: found prior line: %d for line at: %d", curr_num, a:of_line)
  return curr_num
endfunction


" -------------------------------------------------------------------
" match pairs
" -------------------------------------------------------------------
function g:FpcGetPriorPair(of_line, match_open, match_close)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  if curr_num == 0
    return 0
  endif
  let search_from = curr_num
  let search_pair_start = a:match_open
  let search_pair_middle = ''
  let search_pair_end = a:match_close
  DMSG printf("fpc-indent: seeking prior '%s' for '%s' at: %d, boundary at: %d",
        \ search_pair_start, search_pair_end,
        \ curr_num, b:backtrack_boundary)
  let stop_search = b:backtrack_boundary == 0
        \ ? g:FpcGetBacktrackBoundary(curr_num - 1)
        \ : b:backtrack_boundary
  let save_cursor = getcurpos()
  call cursor(curr_num, 1)
  let nesting = 0
  let search_from = curr_num
  DMSG printf("fpc-indent: boundary: %d  stop: %d", b:backtrack_boundary, stop_search)
  " prime the pump
  let where_matched = searchpair(
        \ search_pair_start, search_pair_middle, search_pair_end,
        \ "bW", "", stop_search)
  DMSG printf("fpc-indent: result of of first searchpair from: %d at: %d '%s'",
        \ search_from, where_matched, getline(where_matched))
  while where_matched
    if getline(where_matched) =~ search_pair_end
      let nesting = nesting + 1
      DMSG printf("fpc-indent: nesting+: %d at: %d", nesting, where_matched)
    else
      if nesting == 0 
        DMSG printf("fpc-indent: found at: %d", where_matched)
        break
      endif
      let nesting = nesting - 1
      DMSG printf("fpc-indent: nesting-: %d at: %d", nesting, where_matched)
    endif
    let where_matched = searchpair(
          \ search_pair_start, search_pair_middle, search_pair_end,
          \ "bW", "", stop_search)
    if where_matched > 0
      DMSG printf("fpc-indent: result of searchpair %d '%s'",
            \ where_next, getline(where_next))
    endif
  endwhile

  " put cursor back once searching done
  call cursor(save_cursor[1], save_cursor[2])

  if where_matched <= stop_search || nesting
    DMSG printf("fpc-indent: match for '%s' at: %d not found by: %d, nesting: %d",
          \ search_pair_end, curr_num, stop_search, nesting)
    let where_matched = 0
  endif
  return where_matched
endfunction


" ------------------------------------------------------------------
" helpers to trim lines to simplify analysis.
"
" as strings can't span lines, remove them first. i think this should
" handle realistic nesting. i couldn't get apostrophes for pascal
" string delimiters to work in a very magic regex pattern so i just
" used a normal pattern for those.
"
" as two separate functions to simplify comment analysis above.
" ------------------------------------------------------------------
function! g:FpcStripStrings(orig_line)
  let curr_line = substitute(a:orig_line, "'.*'", '', 'g')
  return curr_line
endfunction

function! g:FpcStripComments(orig_line)
  let curr_line = substitute(a:orig_line, '\v\(\*.*\*\)', '', 'g')
  let curr_line = substitute(curr_line,   '\v\{.*\}', '', 'g')
  let curr_line = substitute(curr_line,   '\v\/\/.*$', '', 'g')
  return curr_line
endfunction


" ------------------------------------------------------------------
" i think it's more traditional to put these in the ftplugin directory
" but let's start from here ...
"
" wrap selected lines (be in V not v) with the indent off and indent
" on directives. using the wrap function avoids a warning for a
" mismatch of directives.
"
" txb: needs better placement and scoping of code, customize keybind.
" ------------------------------------------------------------------
function! g:FpcWrapIndentOffOn()
  call setreg("", '{##indent:off}' .. "\n" .. getreg("") .. '{##indent:on}' .. "\n")
endfunction
" delete selection (should be in visual line mode), wrap in directives,
" paste before .
vnoremap <localleader>wi x:call g:FpcWrapIndentOffOn()<enter>""P


" ------------------------------------------------------------------
" test helpers
" ------------------------------------------------------------------
function! g:FpcIsVarOrType(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  if curr_num == 0
    return 0
  endif
  let curr_line = line(curr_num)
  if curr_line =~ '\v\c^\s*[a-z_][a-z0-9_]*>\s*\='
    return 1
  endif
  if curr_line =~ '\v\c^\s*[a-z_][a-z0-9_]*>\s*\:[^=]'
    return 2
  endif
  return 0
endfunction
 
function! FpcTestMatch(seeking)
  let i = 1
  let j = 0
  let keep_dlfag = g:DMSG_flag
  let g:DMSG_flag = 1
  DMSG printf("fpc-indent: lines matching %s ...", a:seeking)
  while i <= line('$')
    let s = getline(i)
    if s =~ a:seeking
      DMSG printf("%d:%s", i, s)
      let j = j + 1
    endif
    let i = i + 1
  endwhile
  DMSG printf("fpc-indent: %d lines checked, %d lines matched", (i - 1), k)
  let g:DMSG_flag = keep_dlfag
endfunction


" ------------------------------------------------------------------
"  provide undo for this indent and restore any settings that we
"  should.
" ------------------------------------------------------------------
if !exists('b:undo_indent')
  let b:undo_indent = ''
else
  let b:undo_indent = '|'
endif
let b:undo_indent .= '
      \ setlocal indentexpr< indentkeys<
      \'
let &cpo = s:save_cpo
unlet s:save_cpo


" =====================================================================
" end indent/fpc.vim
" =====================================================================
" vim: ts=2 sts=2 sw=2 et
