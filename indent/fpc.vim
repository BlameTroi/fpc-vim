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
if exists('b:did_indent')
  finish
endif
let b:did_indent = 1


let s:save_cpo = &cpo
set cpo&vim


" ------------------------------------------------------------------
" beyond the b:did and b:undo standards for plugins, these are
" buffer local variables that are global to indenting. hoping
" to remove almost all of these.
"
" i developed a tendency to make everything buffer local scope
" but i now see that as a mistake. yes, it makes some debugging
" easier but it introduces extra state that can cause side
" effects. variables should be narrowly scoped.
"
" any script scoped variable should be treated as a constant.
" ------------------------------------------------------------------
function g:FpcInitBufferVars()
  let b:indenting = 1
  let b:last_indent_request = -1
  " txb: set to 1 to enable DMSG throughout
  "      or do selectively in functions around
  "      trouble spots. DMSGOn and DMSGOff work
  "      there too.
  let g:DMSG_flag = 1
endfunction


" ------------------------------------------------------------------
" indent doesn't fire without an expression.
"
" while indentkeys aren't being used at present, both indentkeys and
" = (filtering) use indentexpr.
"
" note the passing of v:lnum instead of using the global directly in
" FpcGetIndent(). this allows testing from command mode. most 
" functoins are set up to allow interactive testing via call or echo
" and can accept marks as line numbers.
" ------------------------------------------------------------------
call g:FpcInitBufferVars()
setlocal indentexpr=g:FpcGetIndent(v:lnum)


" ------------------------------------------------------------------
" main indent function
"
" this code relies heavily on checking syntax highlighting in the
" hope that 'reusing' the highlighter's pattern matches is faster
" and more accurate that doing new pattern matches.
"
" the ternary expression return is an optimistic optimization in
" the hope that it might save vim/nvim a few cycles.
" ------------------------------------------------------------------
function! g:FpcGetIndent(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line

  " invalid lines always indent 0
  if curr_num < 1 || curr_num > line('$')
    return 0
  endif

  " attempt to avoid broken indenting or bad nesting of
  " indent off/on directives from messing up indent
  if b:last_indent_request == -1 ||
        \ b:last_indent_request >= curr_num
    let b:indenting = 1
  endif
  let b:last_indent_request = curr_num

  let curr_line = getline(curr_num)
  let curr_indent = indent(curr_num)

  " preprocessor commands ALWAYS reset their own indent.
  " here is where to process indent directives. b:indenting
  " is only modified by directive in FpcIndentOffOnFlagging
  " but it defaults to 1 when the plugin is loaded and
  " (hopefully) when a new full indent pass is started.
  if g:FpcIsPreprocessing(curr_num, curr_line)
    call g:FpcIndentOffOnFlagging(curr_num, curr_line)
    return curr_indent == 0 ? -1 : 0
  endif

  " a lines indent is unchanged if indenting is off, it is a
  " comment, or it is blank
  if !b:indenting ||
        \ g:FpcIsComment(curr_num) ||
        \ curr_line =~# '\v(^$|^\s*$)' 
    return -1
  endif

  " this line may be indentable.
  "
  " new statements are expected to start on a new line, so the
  " first word on a line tells us how to indent it. for our
  " purposes, 'word' is the name of the syntax highlight group
  " from syntax/fpc.vim. 
  let curr_word = g:FpcGetFirstWordHl(curr_num)

  " these lines all have hard left justification and are boundary
  " lines when looking backward for indent guidance.
  if g:FpcIsBoundaryWordHl(curr_word)
    return curr_indent == 0 ? -1 : 0
  endif

  " indenting drives off a prior line. sometimes it's the
  " immediately prior line, others it's a specific line 
  " but we'll start with the immediate prior line.
  let prev_num = g:FpcGetPrior(curr_num)
  let prev_word = g:FpcGetFirstWordHl(prev_num)
  let prev_indent = indent(prev_num)

  " an outdenting word is one that will cause itself and following
  " lines to outdent. end should line up under begin, and until
  " should line up under repeat.

  " every end has its beginning
  " txb: and record, but ignoring that for now.
  if curr_word =~# 'fpcEnd'
    let prev_num = g:FpcGetPriorPair(curr_num, '\v\c<begin>', '\v\c<end>')
    let prev_word = g:FpcGetFirstWordHl(prev_num)
    let prev_indent = indent(prev_num)
    return curr_indent == prev_indent - &shiftwidth
          \ ? -1
          \ : prev_indent - &shiftwidth
  endif

  " every until has its repeat
  if curr_word =~# 'fpcUntil'
    let prev_num = g:FpcGetPriorPair(curr_num, '\v\c<begin>', '\v\c<end>')
    let prev_word = g:FpcGetFirstWordHl(prev_num)
    let prev_indent = indent(prev_num)
    return curr_indent == prev_indent - &shiftwidth
          \ ? -1
          \ : prev_indent - &shiftwidth
  endif

  " begin shows up all over the code, but basically lines up
  " under the statement it provides a grouping for. if, for, etc.
  if curr_word ==# 'fpcBegin'
    while !g:FpcIsBoundaryLine(prev_num) && prev_word !~# '\vfpc(For|While|If|Then|Else|With|Do|End)'
      let prev_num = g:FpcGetPrior(prev_num)
      let prev_word = g:FpcGetFirstWordHl(prev_num)
      let prev_indent = indent(prev_num)
    endwhile
    return curr_indent == prev_indent ? -1 : prev_indent
  endif

  " if the line does not begin with an indenting word, it is
  " likely either a continuation line or the start of a statement
  " which should be a function call or an assignment.
  "
  " take the indent of the prior code line if the prior line is
  " also not an indenting word. Otherwise, things get more 
  " interesting.
  if !g:FpcIsIndentingWordHl(prev_word)
    return curr_indent == prev_indent ? -1 : prev_indent
  endif

  " these aren't really indenting words at this point.
  if prev_word =~# '\vfpc(End|Until)'
    return curr_indent == prev_indent ? -1 : prev_indent
  endif

  " these words indent one level. while multiple could be on
  " any one line, the indent is only one level.
  if prev_word =~# '\vfpc(Begin|Label|Const|Type|Var|If|For|While|With|Repeat|Case|Then|Else|Uses|Record)'
    return curr_indent == prev_indent + &shiftwidth
          \ ? -1
          \ : prev_indent + &shiftwidth
  endif

  " if we get here, it's a hole in the bucket situation
  DMSG printf('FpcGetIndent: lines unrecognized at current: %d %s previous: %d %s', curr_num, curr_word, prev_num, prev_word)
  return curr_indent == prev_indent ? -1 : prev_indent

endfunction


" -------------------------------------------------------------------
" is a line a preprocesosr directive? {$...} (*$...*) {##...} #
" -------------------------------------------------------------------
function! g:FpcIsPreprocessing(curr_num, curr_line)
  return a:curr_line =~# '\v^\s*(((\{|\(\*)(\$|\#\#))|\#)'
endfunction


" -------------------------------------------------------------------
" format/indent directives {##indent:on|off} can turn off
" indent processing for a block of lines. defaults to on.
" the indenting flag b:indenting is a global.
" -------------------------------------------------------------------
function! FpcIndentOffOnFlagging(curr_num, curr_line)
  if a:curr_line =~? '\v^\s*((\{\#\#|\(\*\#\#))indent'
    let b:indenting = -1
    if a:curr_line =~? '\v<indent[: =-]off>'
      let b:indenting = 0
    endif
    if a:curr_line =~? '\v<indnet[: =-]on>'
      let b:indenting = 1
    endif
    if b:indenting == -1
      echoerr printf('FpcGetIndent: bad ##indent directive at line: %d %s', a:curr_num, a:curr_line)
      let b:indenting = 1
      echoerr printf('FpcGetIndent: forcing indent on')
    endif
  endif
endfunction


" -------------------------------------------------------------------
" use the syntax highlighting name of the first non blank character
" on the line to identify the word. highlighting of words that can
" influence indent and structure is finely grained in the syntax
" file.
" -------------------------------------------------------------------
function! g:FpcGetFirstWordHl(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  if curr_num == 0
    return 'ERROR-UNKNOWN'
  endif
  let result = 0
  let save_cursor = getcurpos()
  call cursor(curr_num, 1)
  " if we don't find a char on the same line, it's an error
  let at_line = search('\v\S', 'cW', line('$'), 1000)
  if at_line != curr_num
    let result = 'ERROR-UNKNOWN'
  else
    let result = synIDattr(synID(line('.'),col('.'),0),'name')
  endif
  call cursor(save_cursor[1], save_cursor[0])
  return result
endfunction


" -------------------------------------------------------------------
" find a hard boundary line to end backward searches. these lines
" mark the start of code units such as procedures or sections in
" a unit.
" -------------------------------------------------------------------

let s:match_boundary_line = '\v\c^\s*<(procedure|function|label|var|program|unit|uses|const|type|interface|implementation|initialization|finalization)>'

function! g:FpcFindSearchBoundary(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  if curr_num == 0
    return 0
  endif
  let save_cursor = getcurpos()
  call cursor(curr_num, 1)
  let prior_num = search(s:match_boundary_line, 'bW', 1, 0, s:skip_contained)
  if prior_num < 1
    DMSG printf('no boundary found before %d', curr_num)
    let prior_num = 0
  else
    DMSG printf('boundary for %d found at %d,%d %s',
          \ curr_num,
          \ line('.'), col('.'),
          \ synIDattr(synID(line('.'),col('.'),0), 'name'))
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
" all indenting word highlights, kept in alphabetical order please!
" -------------------------------------------------------------------
let s:fpc_indenting_words = [
      \ 'fpcBegin',
      \ 'fpcCase',
      \ 'fpcConst',
      \ 'fpcDo',
      \ 'fpcElse',
      \ 'fpcEnd',
      \ 'fpcFinalization',
      \ 'fpcFor',
      \ 'fpcFunction',
      \ 'fpcGoto',
      \ 'fpcIf',
      \ 'fpcImplementation',
      \ 'fpcInitialization',
      \ 'fpcInterface',
      \ 'fpcLabel',
      \ 'fpcProcedure',
      \ 'fpcProgram',
      \ 'fpcRepeat',
      \ 'fpcThen',
      \ 'fpcType',
      \ 'fpcUnit',
      \ 'fpcUntil',
      \ 'fpcVar',
      \ 'fpcWhile',
      \ 'fpcWith',
      \ ]

" -------------------------------------------------------------------
" check current highlight to see if it is an indenting word
" -------------------------------------------------------------------
function! g:FpcIsIndentingWordHl(word)
  for item in s:fpc_indenting_words
    if item ==# a:word
      return 1
    endif
    if item ># a:word
      return 0
    endif
  endfor
  return 0
endfunction

" -------------------------------------------------------------------
" a boundary word is one that marks a significant structural point
" in pascal source code.
" -------------------------------------------------------------------
function! g:FpcIsBoundaryWordHl(word)
  return a:word =~# '\vfpc(Program|Unit|Procedure|Function|Const|Type|Var|Label|Uses|Interface|Implementation|Initialization|Finalization)'
endfunction


" -------------------------------------------------------------------
" a boundary line is one that ends a backward search. 
" -------------------------------------------------------------------
function! g:FpcIsBoundaryLine(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  return curr_num < 1 ||
        \ curr_num > line('.') ||
        \ g:FpcIsBoundaryWordHl(g:FpcGetFirstWordHl(curr_num))
endfunction


" -------------------------------------------------------------------
" line is blank or has a leading comment
" -------------------------------------------------------------------
function! g:FpcIsBlankOrLeadingComment(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  let curr_line = getline(curr_num)
  return curr_line =~# '\v^\s*$' ||
        \ curr_line =~# '\v^\s*((\{.*\})|(\(\*.*\*\))|\/\/)\.*$'
endfunction


" -------------------------------------------------------------------
" find prior code line 
" -------------------------------------------------------------------
function! g:FpcGetPrior(of_line)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line

  if curr_num <= 1
    return 0
  endif

  let prev_num = prevnonblank(curr_num - 1) 
  let prev_word = g:FpcGetFirstWordHl(prev_num)
  while prev_num != 0 && prev_word =~# '\vfpc(Comment|String|Operator|Integer|Real|Delimiter)'
    let prev_num = prevnonblank(prev_num - 1) 
    let prev_word = g:FpcGetFirstWordHl(prev_num)
  endwhile

  return prev_num
endfunction



" -------------------------------------------------------------------
" match pairs
" -------------------------------------------------------------------
let s:frag_hl_name = 'synIDattr(synID(line(''.''),col(''.''),0),''name'')'
let s:frag_contained = '''\v\cfpc(comment|string)'''
let s:skip_contained = s:frag_hl_name .. '=~#' .. s:frag_contained
let s:accept_contained = '!(' .. s:skip_contained .. ')'

function g:FpcIsComment(of_line)
  " trust FpcIsHlChar to deal with a:of_line
  return g:FpcIsHlChar(a:of_line, '\vfpcComment')
endfunction

function g:FpcIsHlChar(of_line, desired_hl)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  if curr_num < 1 || curr_num > line('$')
    "DMSG printf('FpcIsHlChar: invalid line in call at: %d %s', curr_num, a:desired_hl)
    return 1
  endif
  let result = 0
  let save_cursor = getcurpos()
  call cursor(curr_num, 1)
  " search({pattern} [, {flags} [, {stopline} [, {timeout} [, {skip}]]]])
  " txb: ignoring possibility of error on search right now
  call search('\v.', 'cW', line('$'), 1000)
  let result = synIDattr(synID(line('.'),col('.'),0),'name') =~? a:desired_hl
  "DMSG printf('FpcIsHlChar: result: %d, dot: %d,%d, cursor: %s', result, line('.'), col('.'), getcurpos())
  "DMSG printf('FpcIsHlChar: want: %s got : %s', a:desired_hl, synIDattr(synID(line('.'),col('.'),0),'name'))
  call cursor(save_cursor[1], save_cursor[0])
  return result
endfunction

function g:FpcGetPriorPair(of_line, match_open, match_close)
  let curr_num = type(a:of_line) == v:t_string ? line(a:of_line) : a:of_line
  if curr_num == 0
    return 0
  endif
  let search_from = curr_num
  let search_pair_start = a:match_open
  let search_pair_middle = ''
  let search_pair_end = a:match_close
  let stop_search = g:FpcFindSearchBoundary(curr_num - 1)
  let save_cursor = getcurpos()
  call cursor(curr_num, 1)
  let nesting = 0
  let search_from = curr_num
  " prime the pump
  let where_matched = searchpair(
        \ search_pair_start, search_pair_middle, search_pair_end,
        \ 'bW', s:skip_contained, stop_search)
  DMSG printf('FpcGetPriorPair: result of of first searchpair from: %d at: %d %s',
        \ search_from, where_matched, getline(where_matched))
  while where_matched
    if g:FpcStripComments(g:FpcStripStrings(getline(where_matched))) =~ search_pair_end
      let nesting = nesting + 1
      DMSG printf('FpcGetPriorPair: nesting+: %d at: %d', nesting, where_matched)
    else
      if nesting == 0 
        DMSG printf('FpcGetPriorPair: found at: %d', where_matched)
        break
      endif
      let nesting = nesting - 1
      DMSG printf('FpcGetPriorPair: nesting-: %d at: %d', nesting, where_matched)
    endif
    let where_matched = searchpair(
          \ search_pair_start, search_pair_middle, search_pair_end,
          \ 'bW', s:skip_contained, stop_search)
    if where_matched > 0
      DMSG printf('FpcGetPriorPair: result of searchpair %d %s',
            \ where_next, getline(where_next))
    endif
  endwhile

  " put cursor back once searching done
  call cursor(save_cursor[1], save_cursor[2])

  if where_matched <= stop_search || nesting
    DMSG printf('FpcGetPriorPair: match for %s at: %d not found by: %d, nesting: %d',
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
  call setreg('', '{##indent:off}' .. '\n' .. getreg('') .. '{##indent:on}' .. '\n')
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
  DMSG printf('FpcTestMatch: lines matching %s ...', a:seeking)
  while i <= line('$')
    let s = getline(i)
    if s =~ a:seeking
      DMSG printf('%d:%s', i, s)
      let j = j + 1
    endif
    let i = i + 1
  endwhile
  DMSG printf('FpcTestMatch: %d lines checked, %d lines matched', (i - 1), k)
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
