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
"
" identifying keywords can be done with pattern matching, but after
" going down that path i have switched to using syntax highlights
" to identify keywords. the vim highlighting works well and is likely
" more accurate than my twisted regular expressions would be.
"
" keywords that influence code format and indent are given their
" own highlight name in the syntax definiton. fpcIf, fpcThen, fpcDo,
" fpcBegin, fpcEnd, and so on.
" ===================================================================


" Only load this indent file when no other was loaded.
if exists('b:did_indent')
  finish
endif
let b:did_indent = 1


let s:save_cpo = &cpo
set cpo&vim

let g:FpcIndentProcedureDefinitions = 0
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
" pascal is freeform but indenting and formatting for readability
" is line oriented. to keep things manageable this code assumes
" that new statements always begin on a new line. a statement may
" span multiple lines.
"
" since new statements are expected to start on a new line, the
" first word on a line tells us how to indent it. for our
" purposes, 'word' is the name of the syntax highlight group
" from syntax/fpc.vim.
"
" following removed for debugging, but i may add back later:
"
"   return curr_indent == desired_indent ? -1 : desired_indent
" replaced with
"   return desired_indent
" the ternary expression return is an optimistic optimization in
" the hope that it might save vim/nvim a few cycles.
" ------------------------------------------------------------------
function! g:FpcGetIndent(for_lnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum

  " invalid lines always indent 0
  if curr_lnum < 1 || curr_lnum > line('$')
    return 0
  endif

  " attempt to avoid broken indenting or bad nesting of
  " indent off/on directives from messing up indent
  if b:last_indent_request == -1
        \ || b:last_indent_request >= curr_lnum
    let b:indenting = 1
  endif
  let b:last_indent_request = curr_lnum

  let [curr_line, curr_indent] = [getline(curr_lnum), indent(curr_lnum)]

  " preprocessor commands ALWAYS reset their own indent.
  " here is where to process indent directives. b:indenting
  " is only modified by directive in FpcIndentOffOnFlagging.
  if g:FpcIsPreprocessing(curr_lnum, curr_line)
    call g:FpcIndentOffOnFlagging(curr_lnum, curr_line)
    return 0
  endif

  " indent is unchanged if indenting is off or the line is
  " a blank or comment line
  if !b:indenting || curr_line =~# '\v(^$|^\s*$)' || g:FpcIsComment(curr_lnum)
    return -1
  endif

  " this line may be indentable.
  let curr_word = g:FpcGetFirstWord(curr_lnum)

  " these lines all have hard left justification and are boundary
  " lines when looking backward for indent guidance.
  if g:FpcIsBoundaryWord(curr_word)
    if !g:FpcIndentProcedureDefinitions && curr_word =~# '\vfpc(Procedure|function)'
      return -1
    endif
    return 0
  endif

  " indenting drives off a prior line. sometimes it's the line
  " immediately prior, others it's a specific line.
  let prev_num = g:FpcGetPrior(curr_lnum)
  let [prev_word, prev_indent] = [g:FpcGetFirstWord(prev_num), indent(prev_num)]

  " do not rearrange function/procedure definitions that may be
  " pretty-printed for documentation formatting. 
  if prev_word =~# '\vfpc(Procedure|Function)'
        \ && !g:FpcIndentProcedureDefinitions 
        \ && curr_lnum <= g:FpcLastLineOfProcedureDef(prev_num)
    return -1
  endif

  " an outdenting word is one that will cause itself and following
  " lines to outdent. end should line up under begin, and until
  " should line up under repeat.

  " every end has its beginning
  "  or record ...
  if curr_word =~# '\vfpc(End|Then|Else|Until)'
    let prev_num = g:FpcGetPriorPair(curr_lnum, curr_word)
    let prev_word = g:FpcGetFirstWord(prev_num)
    let prev_indent = indent(prev_num)
    "echomsg printf('FpcGetIndent: %s at: %d found prior: %s at: %d indent: %d', curr_word, curr_lnum, prev_word, prev_num, prev_indent)
    return prev_indent
  endif


  " begin shows up all over the code, but basically lines up
  " under the statement it provides a grouping for. if, for, etc.
  " fpcDo is deliberately excluded. stmt<nl>do<nl>begin<nl>...
  " is poor style.
  if curr_word ==# 'fpcBegin' 
    while prev_word !~# '\vfpc(For|While|If|Then|Else|With|End)' && !g:FpcIsBoundaryLine(prev_num)
      let prev_num = g:FpcGetPrior(prev_num)
      let prev_word = g:FpcGetFirstWord(prev_num)
      let prev_indent = indent(prev_num)
    endwhile
    return prev_indent
  endif

  " while taking the first word on a line works well, it doesn't
  " for record/object/class. since these all need to trigger an
  " indent, a bit of ugliness here goes looking for these as
  " the last word on the prior line. class and object can inherit
  " and name the ancestor as in object(parent), so we'll need
  " to handle that at some point.

  let prev_line = g:FpcStripComments(g:FpcStripStrings(getline(prev_num)))
  if !g:FpcIsIndentingWord(prev_word)
    if prev_line =~? '\v<record$'
      let prev_word = 'fpcRecord'
    elseif prev_line =~? '\v<object$'
      let prev_word = 'fpcObject'
    elseif prev_line =~? '\v<class$'
      let prev_word = 'fpcClass'
    endif
  endif

  " if the line does not begin with an indenting word, it is
  " likely either a continuation line or the start of a statement
  " which should be a function call or an assignment.

  " take the indent of the prior code line if the prior line is
  " also not an indenting word. Otherwise, things get more 
  " interesting.
  if !g:FpcIsIndentingWord(prev_word)
    return prev_indent
  endif

  " these aren't really indenting words at this point.
  if prev_word =~# '\vfpc(End|Until)'
    return prev_indent
  endif

  " these words indent one level. while multiple could be on
  " any one line, the indent is only one level. record, then,
  " do, else should usually be on the same line as the
  " statement they are a part of, while-do, if-then, and so
  " on. however, they may appear as the first word of a line
  " so we handle them here as well.
  if prev_word =~# '\vfpc(Begin|Label|Const|Type|Var|If|For|While|With|Repeat|Case|Then|Else|Uses|Record|Do|Object|Class)'
    return prev_indent + &shiftwidth
  endif

  " record, begin, and do sometimes suffix a continuation
  " statement. use simple matching to see if we have that
  " siuation. note that this is a text match against the
  " actual previoius line, not its highlighting. 
  let prev_line = g:FpcStripComments(g:FpcStripStrings(getline(prev_num)))
  if prev_line =~? '\v<(then|else|do|begin|record|object|class)$'
    "echomsg printf("FpcGetIndent: trailing then/else/do/beign/record seen at: %d for: %d', prev_num, curr_lnum)
    return prev_indent + &shiftwidth
  endif

  " if we get here, it's a hole in the bucket situation
  echoerr printf('FpcGetIndent: lines unrecognized at current: %d %s previous: %d %s', curr_lnum, curr_word, prev_num, prev_word)
  return prev_indent

endfunction


" returns end line and column of the end of a procedure or
" function definition. can specify a column to start in but
" that defaults to 1. cursor preserved. returns 0 on error.
"
" a function is a procedure that returns a value. neither
" requires parameters in their definition. either can have
" procedural paraemters.
"
" procedure flush;
" procedure recycle(q: string);
" function x(var i: integer; function y(z:real):real;
"           j,k: integer): boolean;
"
" txb: if the first word on a new line in a parameter
"      list is function or procedure, we loop. for now
"      just stop the loop and return 0 for error.
" txb: better fix will be to remember the last line
"      but i'm not sure about state at all times so
"      this works for now.
" 
" in all cases, the end of a definitin is found when the
" trailing semicolon is reached. 
function! g:FpcLastLineOfProcedureDef(for_lnum, for_cnum = 1)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  let curr_cnum = type(a:for_cnum) == v:t_string ? col(a:for_cnum) : a:for_cnum

  " sanity check
  let first_word = g:FpcGetFirstWord(curr_lnum)
  if first_word !~# '\vfpc(Procedure|Function)'
    return [0, 0]
  endif

  let save_cursor = getcurpos()
  call cursor(curr_lnum, curr_cnum)

  let s = search('\v\c<(procedure|function)>', 'cezW', 0, 0, s:skip_contained)
  if s == 0
    call cursor(save_cursor[1], save_cursor[2])
    return [0, 0]
  endif

  let still_searching = 1
  let nesting = 0
  while still_searching
    let s = search('\v(\;)|(\()|(\))', 'zpW', 0, 0, s:skip_contained)
    " 2 is ;, 3 is (, 4 is ) 
    if s == 2 && !nesting
      let result = line('.')
      break
    endif
    if s == 3
      let nesting = nesting + 1
    endif
    if s == 4
      let nesting = nesting - 1
      if nesting < 0
        let result = 0
        break
      endif
    endif
  endwhile

  call cursor(save_cursor[1], save_cursor[2])
  return result
endfunction

" search({pattern} [, {flags} [, {stopline} [, {timeout} [, {skip}]]]])
"		'b'	search Backward instead of forward
"		'c'	accept a match at the Cursor position
"		'e'	move to the End of the match
"		'p'	return number of matching sub-Pattern instead of line number 
"		    (sub1)|(sub2) ... returns 2 for sub1, 3 for sub2 1 if entire match but
"		    no specifc sub matched
"		'W'	don't Wrap around the end of the file
"		'z'	start searching at the cursor column instead of Zero
" return list [ highlight of first word, start row, col, end row, col]
"
" finding statement start ok on if and deep in a proc, but there
" are issues at front of proc
"
" also, col numbers are looking weird, need to trace
function! g:FpcStatementStart(for_lnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  let save_cursor = getcurpos()
  call cursor(curr_lnum, 1)
  let result_first_word = 'UNKNOWN'
  let result_start_line = 0
  let result_start_col = 0
  let result_end_line = 0
  let result_end_col = 0
  let semi_at = search('\v\;', 'bW', 0, 1000, s:skip_contained)
  if semi_at == 0
    call cursor(save_cursor[1], save_cursor[2])
    return [result_first_word, 0, 0, 0, 0]
  endif
  let result_end_line = line('.')
  let result_end_col = col('.')
  let semi_at = search('\v\;', 'bW', 0, 1000, s:skip_contained)
  if semi_at == 0
    call cursor(save_cursor[1], save_cursor[2])
    return [result_first_word, 0, 0, result_end_line, result_end_col]
  endif
  let semi_at = search('\v\S', 'zW', curr_lnum, s:skip_contained)
  if semi_at == 0
    call cursor(save_cursor[1], save_cursor[2])
    return [result_first_word, 0, 0, result_end_line, result_end_col]
  endif
  let result_start_line = line('.')
  let result_start_col = col('.')
  call cursor(save_cursor[1], save_cursor[2])
  let result_first_word = g:FpcGetWordAt(result_start_line, result_start_col)
  return [result_first_word, result_start_line, result_start_col, result_end_line, result_end_col]
endfunction


function! g:FpcTestSearch(for_lnum, for_cnum, for_pat, for_flag)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  let curr_col = type(a:for_cnum) == v:t_string ? col(a:for_cnum) : a:for_cnum
  let save_cursor = getcurpos()
  call cursor(curr_lnum, curr_col)
  let start_cursor = getcurpos()
  let stop_line = a:for_flag =~# 'b' ? 1 : line('$')
  let result = search(a:for_pat, a:for_flag, stop_line, 1000, s:skip_contained)
  echomsg printf('FpcTestSearch: search %s %s %d,%d %d %d,%d %s',
        \ a:for_pat, a:for_flag, start_cursor[1], start_cursor[2],
        \ result, line('.'), col('.'),
        \ g:FpcGetWordAt(line('.'), col('.')))
  call cursor(save_cursor[1], save_cursor[2])
endfunction

function! g:FpcStatementEnd(for_lnum)
  return -1
endfunction

" -------------------------------------------------------------------
" is a line a preprocesosr directive? {$...} (*$...*) {##...} #
" -------------------------------------------------------------------
function! g:FpcIsPreprocessing(curr_lnum, curr_line)
  return a:curr_line =~# '\v^\s*(((\{|\(\*)(\$|\#\#))|\#)'
endfunction


" -------------------------------------------------------------------
" format/indent directives {##indent:on|off} can turn off
" indent processing for a block of lines. defaults to on.
" the indenting flag b:indenting is a global.
" -------------------------------------------------------------------
function! FpcIndentOffOnFlagging(for_lnum, for_line)
  if a:for_line =~? '\v^\s*((\{\#\#|\(\*\#\#))indent'
    let b:indenting = -1
    if a:for_line =~? '\v<indent[: =-]off>'
      let b:indenting = 0
    endif
    if a:for_line =~? '\v<indnet[: =-]on>'
      let b:indenting = 1
    endif
    if b:indenting == -1
      echoerr printf('FpcGetIndent: bad ##indent directive at line: %d %s, forcing indent on!', a:for_lnum, a:for_line)
      let b:indenting = 1
    endif
  endif
endfunction


" -------------------------------------------------------------------
" use the syntax highlighting name of the first non blank character
" on the line to identify the word. highlighting of words that can
" influence indent and structure is finely grained in the syntax
" file.
"
" these pattern for highlight name is fpcTheword, so an if would be
" fpcIf, then would be fpcThen, and so on. See syntax/fpc.vim for
" the complete list.
" -------------------------------------------------------------------
function! g:FpcGetFirstWord(for_lnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  if curr_lnum == 0
    return 'ERROR-UNKNOWN'
  endif
  let result = 0
  let save_cursor = getcurpos()
  call cursor(curr_lnum, 1)
  " if we don't find a char on the same line, it's an error
  let at_line = search('\v\S', 'cW', curr_lnum)
  if at_line != curr_lnum
    let result = 'ERROR-UNKNOWN'
  else
    let result = synIDattr(synID(line('.'),col('.'),0),'name')
  endif
  call cursor(save_cursor[1], save_cursor[2])
  return result
endfunction

function! g:FpcGetWordAt(for_lnum, for_cnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  let curr_cnum = type(a:for_cnum) == v:t_string ? col(a:for_cnum) : a:for_cnum
  return  synIDattr(synID(curr_lnum, curr_cnum, 0), 'name')
endfunction
  
" find first word (nonspace character not in string or
" comment) after a specific line, col. the cursor is
" not preserved. the 'word' is the syntax highlight
" name for the character. 
function! g:FpcCursorStartNextWord(for_lnum, for_cnum = 1)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  if curr_lnum == 0 || curr_lnum > line('$') || curr_cnum < 1
    return 'ERROR-UNKNOWN'
  endif
  let result = 0
  call cursor(curr_lnum, curr_cnum)
  let at_line = search('\v\S', 'czW', 0, 0, s:skip_contained)
  if at_line == 0
    let result = 'ERROR-UNKNOWN'
  else
    let result = synIDattr(synID(line('.'), col('.'), 0), 'name')
  endif
  return result
endfunction


" -------------------------------------------------------------------
" find a hard boundary line to end backward searches. these lines
" mark the start of code units such as procedures or sections in
" a unit. returns 0 on error or not found.
"
" some class directives may need to be added here. public, private,
" static?
" -------------------------------------------------------------------

let s:match_boundary_line = '\v\c^\s*<(procedure|function|label|var|program|unit|uses|const|type|interface|implementation|initialization|finalization)>'

function! g:FpcFindSearchBoundary(for_lnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  if curr_lnum == 0
    return 0
  endif
  let save_cursor = getcurpos()
  call cursor(curr_lnum, 1)
  let prior_num = search(s:match_boundary_line, 'bW', 1, 0, s:skip_contained)
  call cursor(save_cursor[1], save_cursor[2])
  return prior_num > 0 ? prior_num : 0
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
      \ 'fpcClass',
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
      \ 'fpcObject',
      \ 'fpcPrivate',
      \ 'fpcProcedure',
      \ 'fpcProgram',
      \ 'fpcPublic',
      \ 'fpcRecord',
      \ 'fpcRepeat',
      \ 'fpcThen',
      \ 'fpcType',
      \ 'fpcUnit',
      \ 'fpcUntil',
      \ 'fpcUses',
      \ 'fpcVar',
      \ 'fpcWhile',
      \ 'fpcWith',
      \ ]


" -------------------------------------------------------------------
" check current highlight to see if it is an indenting word
" -------------------------------------------------------------------
function! g:FpcIsIndentingWord(word)
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
function! g:FpcIsBoundaryWord(word)
  return a:word =~# '\vfpc(Program|Unit|Procedure|Function|Const|Type|Var|Label|Uses|Interface|Implementation|Initialization|Finalization)'
endfunction


" -------------------------------------------------------------------
" a boundary line is one that ends a backward search. 
" -------------------------------------------------------------------
function! g:FpcIsBoundaryLine(for_lnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  return curr_lnum < 1 ||
        \ curr_lnum > line('.') ||
        \ g:FpcIsBoundaryWord(g:FpcGetFirstWord(curr_lnum))
endfunction


" -------------------------------------------------------------------
" find prior code line 
" -------------------------------------------------------------------
function! g:FpcGetPrior(for_lnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum

  if curr_lnum <= 1
    return 0
  endif

  let prev_num = prevnonblank(curr_lnum - 1) 
  let prev_word = g:FpcGetFirstWord(prev_num)
  while prev_num != 0 && prev_word =~# '\v(Comment|String|Operator|Integer|Real|Delimiter)'
    let prev_num = prevnonblank(prev_num - 1) 
    let prev_word = g:FpcGetFirstWord(prev_num)
  endwhile

  return prev_num
endfunction



" -------------------------------------------------------------------
"  common highlight search patterns.
" -------------------------------------------------------------------
let s:frag_hl_name = 'synIDattr(synID(line(''.''),col(''.''),0),''name'')'
let s:frag_contained = '''\v\cfpc(comment|string)'''
let s:skip_contained = s:frag_hl_name .. '=~' .. s:frag_contained
let s:accept_contained = '!(' .. s:skip_contained .. ')'


" ------------------------------------------------------------------
" check for line (first non blank) being a comment.
" ------------------------------------------------------------------
function g:FpcIsComment(for_lnum)
  return g:FpcGetFirstWord(a:for_lnum) ==# 'fpcComment'
endfunction


" ------------------------------------------------------------------
" 
" ------------------------------------------------------------------
function g:FpcGetPriorPair(for_lnum, for_word)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  if curr_lnum == 0
    return 0
  endif
  let search_from = curr_lnum

  if a:for_word ==# 'fpcEnd'
    " record/object/class is a weird case, it doesn't nest
    " and it'd require bad nesting of the pascal source to
    " hit one after processing a nested begin. treating any
    " of record/object/class as begin at this level just
    " works, so rolling with it.
    let pair_start = '\v\c<(begin|record|object|class)>'
    let pair_middle = ''
    let pair_end = '\v\c<end>'
  elseif a:for_word ==# 'fpcThen'
    let pair_start = '\v\c<if>'
    let pair_middle = ''
    let pair_end = '\v\c<then>'
  elseif a:for_word ==# 'fpcElse'
    let pair_start = '\v\c<if>'
    let pair_middle = ''
    let pair_end = '\v\c<else>'
  elseif a:for_word ==# 'fpcUntil'
    let pair_start = '\v\c<repeat>'
    let pair_middle = ''
    let pair_end = '\v\c<until>'
  else
    echoerr printf('FpcGetPriorPrair: unknown pairing word %s', a:for_word)
    return a:for_lnum - 1
  endif
  let stop_search = g:FpcFindSearchBoundary(curr_lnum - 1)
  let save_cursor = getcurpos()
  call cursor(curr_lnum, 1)
  let nesting = 0
  let search_from = curr_lnum
  " prime the pump
  let where_matched = searchpair(
        \ pair_start, pair_middle, pair_end,
        \ 'bW', s:skip_contained, stop_search)
  while where_matched
    if g:FpcGetWordAt(line('.'), col('.')) ==# a:for_word
      let nesting = nesting + 1
    else
      if nesting == 0 
        break
      endif
      let nesting = nesting - 1
    endif
    let where_matched = searchpair(
          \ pair_start, pair_middle, pair_end,
          \ 'bW', s:skip_contained, stop_search)
  endwhile

  " put cursor back once searching done
  call cursor(save_cursor[1], save_cursor[2])

  if where_matched <= stop_search || nesting
    echoerr printf('FpcGetPriorPair: match for %s at: %d not found by: %d, nesting: %d',
          \ pair_end, curr_lnum, stop_search, nesting)
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
function! g:FpcIsVarOrType(for_lnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  if curr_lnum == 0
    return 0
  endif
  let curr_line = line(curr_lnum)
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
  echomsg printf('FpcTestMatch: lines matching %s ...', a:seeking)
  while i <= line('$')
    let s = getline(i)
    if s =~ a:seeking
      echomsg printf('%d:%s', i, s)
      let j = j + 1
    endif
    let i = i + 1
  endwhile
  echomsg printf('FpcTestMatch: %d lines checked, %d lines matched', (i - 1), k)
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
