" indent/fpc.vim
" ===================================================================
" Vim indent file
" Language:    Pascal as fpc for Free Pascal 3.2.2
" Maintainer:  Troy Brumley <blametroi@gmail.com>
" Last Change: November 2023.
" License:     Vim License.
"
" a pluging with indent support for fpc starting from a blank slate
" trying to come up with something i'll like for the objfpc mode of
" free pascal. existing indenting and formatting for pascal and delphi
" don't do what i want.
"
" hoping for simple implementation of simple rules and clean looking
" pascal.
"
" the rules of indenting:
"
" pascal is freeform but indenting and formatting for readability is
" line oriented. to keep things manageable this code assumes that new
" statements always begin on a new line.
"
" 1. a statement always starts on a new line. 
"    various placements of begin/end are handled along with then/else
"    with an eye towards allman or k&r style formatting.
" 2. structural words always reset indenting to the left margin.
"    these are things like procedure/function.
" 3. pascal compiler directives and preprocessor directives are always
"    aligned to the left margin but unlike structural words they do
"    not reset indenting for subsequent lines.
" 3. comments and blank lines are unchanged.
" 4. a line that starts with a comment is a comment line.
" 5. comments can not nest.
"
" the experience, learnings, and observations from implementation:
"
" identifying keywords can be done with pattern matching, but after
" going down that path i have switched to using syntax highlights
" to identify keywords. the vim highlighting works well and is likely
" more accurate than my twisted regular expressions would be.
"
" keywords that influence code format and indent are given their own
" highlight name in the syntax definiton. fpcIf, fpcThen, fpcDo,
" fpcBegin, fpcEnd, and so on.
"
" since new statements are expected to start on a new line the first
" word on a line tells us how to indent it. remember that 'word' 
" usually means 'the name of the syntax highlight group.'
"
" finally, i started over with this project more times than i kept
" count of. the task is both more interesting and more challenging
" than expected. as i was learning vimscript (viml as neovim would
" have it) new understanding of capabilities of the api led to better
" code.
"
" i take comfort from the comments i read in other plugins by coders
" who also had issues learning some of the api (we're looking at
" you, searchpairpos()).
" ===================================================================


" Only load this indent file when no other was loaded.
if exists('b:did_indent')
  finish
endif
let b:did_indent = 1


let s:save_cpo = &cpo
set cpo&vim


" ------------------------------------------------------------------
" indent doesn't fire without an expression.
"
" while indentkeys aren't being used at present, indentkeys uses
" indentexpr and the = (filtering/formatter) uses it if equalprg is
" not set.
"
" note the passing of v:lnum instead of using the global directly in
" FpcGetIndent(). this allows testing from command mode. most 
" functoins are set up to allow interactive testing via call or echo
" and can accept marks as line numbers.
" ------------------------------------------------------------------
setlocal indentexpr=g:FpcGetIndent(v:lnum)


" ------------------------------------------------------------------
" indent and format options. these should be kept to a minimum and
" moved to the plugin file when we're done.
" ------------------------------------------------------------------

" boolean: should procedure and function definitions be indented?
" defaults to false so that if the definitions are formatted for
" documentation the alignment will be changed.
" txb: should there be a buffer scoped copy of the variable made
"      when the plugin is loaded?
let g:FpcIndentProcedureDefinitions = 0


" ------------------------------------------------------------------
" beyond the b:did and b:undo standards for plugins, these are buffer
" local variables that are global to indenting. hoping to remove
" almost all of these.
"
" i developed a tendency to make everything buffer local scope but i
" now see that as a mistake. yes, it makes some debugging easier but
" it introduces extra state that can cause side effects.
"
" modify buffer scoped variables rarely, script scoped variables
" should be treated as constants.
"
" txb: these are buffer scoped, do i need to unlet them?
" ------------------------------------------------------------------

" boolean: are we indenting? controlled by {##indent directive.
let b:indenting = 1

" line number: 
let b:last_indent_request = -1


" ------------------------------------------------------------------
" string: match patterns
"
" any match pattern involving more than a couple of tokens should be
" put into a script scoped variable and those are collected here.
"
" almost every match uses the very-magic \v flag. i find the syntax
" easier to read and write. most match operators in the code use the
" explicit qualifiers # and ?.
"
" txb: i want to make them const but the plugin loads multiple times
"      in my development setup. 
" ------------------------------------------------------------------

" a boundary word is one that marks a significant structural point in
" pascal source code.  two match patterns are needed, one for use
" with search functions to check synID names, and another to search
" for these words at the head of a line of pascal source.
let s:frag_boundary_word = '(' ..
      \ 'Program|Unit|Procedure|Function|Const' ..
      \ '|Type|Var|Label|Uses|Interface|Implementation' ..
      \ '|Initialization|Finalization' ..
      \ ')'
let s:match_boundary_word = '\vfpc' .. s:frag_boundary_word
let s:match_boundary_line = '\v\c^\s*<' .. s:frag_boundary_word .. '>'

" a small attempt at optimizing the expression based on expected
" frequency of keywords in source and the hope that the search
" is left to right.
let s:match_statement_starters = '\vfpc(' ..
      \ 'If|For|While|With|ExtProcedures|ExtWords' ..
      \ '|IoProcedures|Procedures|Repeat|Until' ..
      \ '|Begin|Case|ExtExcept' ..
      \ ')'

" is a line a preprocessor directive? {$...} (*$...*) {##...} #
" c++ style comments are not included.
let s:match_preprocessing_line = '\v^\s*(((\{|\(\*)(\$|\#\#))|\#)'

" some common highlight search patterns.

" get the highlight id name under cursor
let s:frag_hl_name = 'synIDattr(synID(line(''.''),col(''.''),0),''name'')'

" don't look inside strings or comments in search...()
let s:frag_contained = '''\vfpc(Comment|String)'''
let s:skip_contained = s:frag_hl_name .. '=~#' .. s:frag_contained

" match variable := 
let s:match_identifier_becomes = '\v^\s*\h\w*\s*\:\='

" match case branch label
" what a mess ... any one style works, can't get all three,
" much less allowing for mixing of styles ...
" need to support literal number and string, and possibly
" comma delimited list
"let s:match_case_branch = '\v^\s*\%(' ..
"      \ '\%((\h\w*\s*)(\,\s*\h\w*\s*)*)' ..
"      \ ')\:[^=]'
"      \ '|\%((\d*)(\,\s*\d*)*)' ..
"      \ '|\%((\'.\')(\,\s*\'.\')*)' .. outer ' double pls
"
" match error '\v^\"\s*(\h\w*\s*)\:[^=]'
" '\v^"\s*((\h\w*\s*)(\,\s*\h\w*\s*)*)\:[^=]'
" '\v^"\s*((\d*)(\,\s*\d*)*)\:[^=]'
" '\v^"\s*((\'.\')(\,\s*\'.\')*)\:[^=]'
"case fred of
"'a': writeln;
"'b', 'c': readln;
"9: page;
"7,6: subtract;
"bambam := 7;
"betty : boink;
"wilma: begin .... end;
"dino, bird, dawg: begin .... end;
"else: begin .... end;

" match typename definition
let s:match_typename_definition = '\v([a-zA-Z_][a-zA-Z0-9_]*\s*)\='

" -------------------------------------------------------------------
" an indenting word is a pascal reserved word that influences its own
" indent level, or that of lines that follow it.
"
" these are all indenting word syntax highlight names. this list must
" be kept in alphabetical order. the function g:FpcIsWordInList
" searches this list and counts on the ordering.
" -------------------------------------------------------------------
"  outdent themself and indent following
let s:fpc_bidenting_words = [
      \ 'fpcElse',
      \ 'fpcThen',
      \ ]

" outdent themself and following
let s:fpc_outdenting_words = [
      \ 'fpcEnd',
      \ 'fpcUntil',
      \ ]

" indent following
let s:fpc_indenting_words = [
      \ 'fpcBegin',
      \ 'fpcCase',
      \ 'fpcClass',
      \ 'fpcConst',
      \ 'fpcDo',
      \ 'fpcElse',
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
      \ 'fpcUses',
      \ 'fpcVar',
      \ 'fpcWhile',
      \ 'fpcWith',
      \ ]


" -------------------------------------------------------------------
" check current highlight to see if it is an indenting word. returns
" a true if the highlight name is found in s:fpc_indenting_words.
" -------------------------------------------------------------------
function! g:FpcIsOutdenting(word)
  return g:FpcIsWordInList(a:word, s:fpc_outdenting_words)
endfunction

function! g:FpcIsIndenting(word)
  return g:FpcIsWordInList(a:word, s:fpc_indenting_words)
endfunction

function! g:FpcIsBidenting(word)
  return g:FpcIsWordInList(a:word, s:fpc_bidenting_words)
endfunction

function! g:FpcIsWordInList(word, word_list)
  for item in a:word_list
    if item ==# a:word
      return 1
    endif
    if item ># a:word
      return 0
    endif
  endfor
  return 0
endfunction


" ==================================================================
"
" main indent function
"
" it's a bit busy but it's your basic target for indentexpr.
"
" ==================================================================
function! g:FpcGetIndent(for_lnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum

  " invalid lines always indent 0
  if curr_lnum < 1 || curr_lnum > line('$')
    return 0
  endif

  " don't let broken indenting or bad nesting of indent off/on
  " directives mess things up too badly.
  if b:last_indent_request == -1
        \ || b:last_indent_request >= curr_lnum
    let b:indenting = 1
  endif
  let b:last_indent_request = curr_lnum

  let [curr_line, curr_indent] = [getline(curr_lnum), indent(curr_lnum)]

  " preprocessor directives ALWAYS reset their own indent. indent
  " off/on bracketing of lines is handled in another function where
  " the buffer indent flag can be updated.
  if curr_line =~? s:match_preprocessing_line
    call g:FpcIndentOffOnFlagging(curr_line)
    if b:indenting == -1
      echomsg printf('fpc-indent: bad indent directive at: %d, indent set to on', curr_lnum)
      let b:indenting == 1
    endif
    return 0
  endif

  " if blank, a comment, or indenting is off, the line is unchanged.
  if !b:indenting || curr_line =~# '\v(^$|^\s*$)' || g:FpcIsComment(curr_lnum)
    return -1
  endif

  " this line may be indentable. while every line is not necessarily
  " a new statement, ever statement should be a new line.
  let curr_word = g:FpcGetFirstWord(curr_lnum)

  " any of the boundary words (program, procedure, etc) will reset
  " indenting. an exception is made for a procedure definition to
  " preserve the layout of parameter lists. this can be toggled by
  " a global flag.
  if curr_word =~# s:match_boundary_word
    if !g:FpcIndentProcedureDefinitions && curr_word =~# '\vfpc(Procedure|Function)'
      return -1
    endif
    return 0
  endif

  " indent level for a line is based on the indent of the prior line.
  " sometimes that's the line immediately prior, and others it's a
  " specific line. eg. the opening begin of a procedure lines up under
  " the procedure statement (or const/type/var/label).
  let prev_lnum = g:FpcGetPrior(curr_lnum)
  let [prev_word, prev_indent] = [g:FpcGetFirstWord(prev_lnum), indent(prev_lnum)]
  if prev_word =~# s:match_boundary_word
    let prev_indent = 0
  endif

  " do these lines start new statements?
  let curr_is_new_stmt = g:FpcStartsNewStatement(curr_lnum, curr_word)
  let prev_is_new_stmt = g:FpcStartsNewStatement(prev_lnum, prev_word)

  " do not rearrange function/procedure definitions that may be
  " pretty-printed for documentation formatting. 
  if !g:FpcIndentProcedureDefinitions &&
        \ prev_word =~# '\vfpc(Procedure|Function|Var)'
        \ && curr_lnum <= g:FpcLastLineOfProcedureDef(prev_lnum)[0]
    return -1
  endif

  " an outdenting word is one that will cause itself and following
  " lines to outdent. the easy case is until which lines up under
  " repeat. then and else line up under if. end can close a begin,
  " but also a record/object/class definition. that complexity is
  " tucked away in another function.
  if g:FpcIsOutdenting(curr_word) || g:FpcIsBidenting(curr_word)
    let prev_lnum = g:FpcGetPriorPair(curr_lnum, curr_word)
    let prev_indent = indent(prev_lnum)
    return prev_indent
  endif

  " begin shows up all over the code, but basically lines up under
  " the statement it provides a grouping for. if, for, etc.
  "
  " the space from program or procedure/function to the begin that
  " opens the program or procedure/function statement block needs
  " special handling here to allow for the lack of explicit
  " termination of things such as var or type blocks, and pretty-
  " printing of parameter lists.
  if curr_word =~# '\vfpc(Begin|Const|Var|Type|Label|Do)' 
    while prev_word !~# '\vfpc(For|While|If|Then|Else|With|End)' && !g:FpcIsBoundaryLine(prev_lnum)
      let prev_lnum = g:FpcGetPrior(prev_lnum)
      let prev_word = g:FpcGetFirstWord(prev_lnum)
    endwhile
    " while some pretty printing settings might allow these words to
    " not be on the left margin, they logically are and so the indent
    " for a block opener following them will be set to 0.
    if prev_word =~# '\vfpc(Procedure|Function|Const|Type|Var|Label)'
      return 0
    endif
    return indent(prev_lnum)
  endif

  " while taking the first word on a line works well, it doesn't for
  " record/object/class. since these all need to trigger an indent,
  " a bit of ugliness here to check for one of those being the last
  " word on the prior line. if it is, pretend it was the first word.
  let prev_line = g:FpcStripComments(g:FpcStripStrings(getline(prev_lnum)))
  if !g:FpcIsIndenting(prev_word)
    if prev_line =~? '\v<record$'
      let prev_word = 'fpcRecord'
    elseif prev_line =~? '\v<object$' || prev_line =~ '\v<object\s*\(.*\)$'
      let prev_word = 'fpcObject'
    elseif prev_line =~? '\v<class$' || prev_line =~ '\v<class\s*\(.*\)$'
      let prev_word = 'fpcClass'
    endif
  endif

  " if the line does not begin with an indenting word, it is likely
  " either a continuation line or the start of a statement which
  " should be a function call or an assignment.

  " take the indent of the prior code line if the prior line is also
  " not an indenting word. Otherwise, things get more interesting.
  if !g:FpcIsIndenting(prev_word)
    let [prev_lnum, prev_word] = g:FpcGetPriorStructuredStatement(curr_lnum)
    if prev_lnum && prev_word =~# '\vfpc(While|For|With|If)'
      let prev_indent = indent(prev_lnum)
    endif
    return prev_indent
  endif

  " these aren't really indenting words at this point. they outdent
  " themselves and following lines.
  if g:FpcIsOutdenting(prev_word)
    return prev_indent
  endif

  " these words indent one level. while more than one might be on a
  " line, the indent is only one level. record, then, do, should be
  " on the same line as the statement they are a part of: while-do,
  " if-then, and so on. however, they may appear as the first word of
  " a line so we handle them here as well.
  if g:FpcIsIndenting(prev_word) || g:FpcIsBidenting(prev_word)
    return prev_indent + &shiftwidth
  endif
      
  " if we get here there's a hole in the logic bucket above. complain
  " in a low key manner and take the prior lines indent.
  echomsg printf('lines unrecognized at current: %d [%s] previous: %d [%s]', curr_lnum, curr_word, prev_lnum, prev_word)
  return prev_indent

endfunction


" -------------------------------------------------------------------
" does this line begin a new staement? a new statement is one that
" begins with a pascal keyword that starts a statement (while or for
" but not do or to), is a boundary, is an assignment, or starts with
" a known procedure call (writeln, reset).
"
" txb: should end be here? what about then and else? case branch
"      labels?
" -------------------------------------------------------------------
function! g:FpcStartsNewStatement(for_lnum, for_word)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  return a:for_word =~# s:match_boundary_word || 
        \ a:for_word =~#  s:match_statement_starters ||
        \ (a:for_word =~# '\vfpcIdentifier' &&
        \ getline(curr_lnum) =~# s:match_identifier_becomes)
endfunction

" -------------------------------------------------------------------
" returns end line and column of the end of a procedure or function
" definition. you may specify a column to start searching from, but
" a defualt of 1 is provided.
"
" the original cursor preserved.
"
" returns [0,0] on error.
"
" in pascal a function is a procedure that returns a value. neither
" requires parameters in their definition but either can have
" procedural paraemters.
"
" procedure flush;
" procedure recycle(q: string);
" function x(var i: integer; function y(z:real):real;
"           j,k: integer): boolean;
" 
" in all cases, the end of a definitin is found when the trailing
" semicolon is reached. 
" -------------------------------------------------------------------
" txb: if the first word on a new line in a parameter list is
"      function or procedure, we loop. for now just stop the loop
"      and return 0 for error.
"
"      a better fix will be to remember the last line but i'm not
"      sure about state at all times so this works for now.
" txb: above call needs to recognize both 'var' qualifiers on
"      parameters and procedure reference parameters.
"
"      'var' works most of the time, but will fail if it starts
"      a new line immediately after a line that starts with a
"      procedure reference.
function! g:FpcLastLineOfProcedureDef(for_lnum, for_cnum = 1)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  let curr_cnum = type(a:for_cnum) == v:t_string ? col(a:for_cnum) : a:for_cnum

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
      let result = [line('.'), col('.')]
      break
    endif
    if s == 3
      let nesting = nesting + 1
    endif
    if s == 4
      let nesting = nesting - 1
      if nesting < 0
        let result = [0, 0]
        break
      endif
    endif
  endwhile

  call cursor(save_cursor[1], save_cursor[2])
  return result
endfunction


" -------------------------------------------------------------------
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
" -------------------------------------------------------------------
function! g:FpcTestSearch(for_lnum, for_cnum, for_pat, for_flag)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  let curr_col = type(a:for_cnum) == v:t_string ? col(a:for_cnum) : a:for_cnum
  let save_cursor = getcurpos()
  call cursor(curr_lnum, curr_col)
  let start_cursor = getcurpos()
  let stop_line = a:for_flag =~# 'b' ? 1 : line('$')
  let result = search(a:for_pat, a:for_flag, stop_line, 1000, s:skip_contained)
  echomsg printf('search %s %s %d,%d %d %d,%d %s',
        \ a:for_pat, a:for_flag, start_cursor[1], start_cursor[2],
        \ result, line('.'), col('.'),
        \ g:FpcGetWordAt(line('.'), col('.')))
  call cursor(save_cursor[1], save_cursor[2])
endfunction


" -------------------------------------------------------------------
" format/indent directives {##indent:on|off} can turn off indenting
" for a block of lines. defaults to on. the indenting flag is buffer
" scoped.
" -------------------------------------------------------------------
function! g:FpcIndentOffOnFlagging(for_line)
  if a:for_line =~? '\v^\s*((\{\#\#|\(\*\#\#))indent'
    let b:indenting = -1
    if a:for_line =~? '\v<indent[: =-]off>'
      let b:indenting = 0
    endif
    if a:for_line =~? '\v<indent[: =-]on>'
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
" the pattern for highlight name is fpcTheword, so an if would be
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
  let at_line = search('\v\S', 'cW')
  if at_line != curr_lnum
    let result = 'ERROR-UNKNOWN'
  else
    let result = synIDattr(synID(line('.'),col('.'),0),'name')
  endif
  call cursor(save_cursor[1], save_cursor[2])
  return result
endfunction


" -------------------------------------------------------------------
" get the syntax id name of the word at line,col.
" -------------------------------------------------------------------
function! g:FpcGetWordAt(for_lnum, for_cnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  let curr_cnum = type(a:for_cnum) == v:t_string ? col(a:for_cnum) : a:for_cnum
  return  synIDattr(synID(curr_lnum, curr_cnum, 0), 'name')
endfunction
  
" in pascal, a structured statement is one of compound-statement,
" repetitive-statement, conditional-statement, or with-statement. 
"
" txb: the hack code is fugly but this is the right track.  search pair pos
"      to deal with nesting, but the idea is sound.
"
"      given current statement,
"      find end of prior statement
"      then find beginning of prior statement
"
"      churns a good bit but this is done infrequently i hope. 
function! g:FpcGetPriorStructuredStatement(for_lnum, for_cnum = 1)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  let curr_cnum = type(a:for_cnum) == v:t_string ? col(a:for_cnum) : a:for_cnum
  let result = 'ERROR-UNKNOWN'
  if curr_lnum == 0 || curr_lnum > line('$') || curr_cnum < 1
    echomsg "error in arguments"
    return [0, result]
  endif
  let save_cursor = getcurpos()
  call cursor(curr_lnum, curr_cnum)
  " end; = 2, end = 3, ; = 4, begin = 5, if = 6, for = 7, while = 8, with = 9
  let search_stmt_boundaries = '\v\c(<end\;)|(<end>)|(\;)|(<begin>)|(<if>)|(<for>)|(<while>)|(<with>)'
  let back_one = searchpos(search_stmt_boundaries, 'bepzW', 0, 0, s:skip_contained)
  if back_one == [0, 0]
    echomsg "error not found end prior stmt 1"
    call cursor(save_cursor[1], save_cursor[2])
    return [0, result]
  endif
  let back_two = searchpos(search_stmt_boundaries, 'bepzW', 0, 0, s:skip_contained)
  if back_two == [0, 0]
    echomsg "error not found prior end stmt 2"
    call cursor(save_cursor[1], save_cursor[2])
    return [0, result]
  endif
  if back_two[2] > 4
    if back_two[2] == 5
      let result = 'fpcBegin'
    elseif back_two[2] == 6
      let result = 'fpcIf'
    elseif back_two[2] == 7
      let result = 'fpcFor'
    elseif back_two[2] == 8
      let result = 'fpcWhile'
    elseif back_two[2] == 9
      let result = 'fpcWith'
    endif
    "echomsg printf('back one: %d %d %d %s', back_one[0], back_one[1], back_one[2], g:FpcGetWordAt(back_one[0], back_one[1]))
    "echomsg printf('back one: %d %d %d %s', back_two[0], back_two[1], back_two[2], g:FpcGetWordAt(back_two[0], back_two[1]))
    "echomsg printf('first word prior statement %s', result)
    call cursor(save_cursor[1], save_cursor[2])
    return [back_two[0], result]
  endif
  let result = g:FpcGetFirstWord(back_two[0] + 1)
  "echomsg printf('back one: %d %d %d %s', back_one[0], back_one[1], back_one[2], g:FpcGetWordAt(back_one[0], back_one[1]))
  "echomsg printf('back one: %d %d %d %s', back_two[0], back_two[1], back_two[2], g:FpcGetWordAt(back_two[0], back_two[1]))
  "echomsg printf('first word prior statement %s', result)
  call cursor(save_cursor[1], save_cursor[2])
  return [back_two[0] + 1, result]
endfunction


" -------------------------------------------------------------------
" find first word (nonspace character not in string or comment) after
" a specific line, col. the cursor is not preserved. 'word' is the
" syntax highlight name for the character. 
" -------------------------------------------------------------------
"function! g:FpcCursorStartNextWord(for_lnum, for_cnum = 1)
"  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
"  let curr_cnum = type(a:for_cnum) == v:t_string ? col(a:for_cnum) : a:for_cnum
"  if curr_lnum == 0 || curr_lnum > line('$') || curr_cnum < 1
"    return 'ERROR-UNKNOWN'
"  endif
"  let result = 0
"  call cursor(curr_lnum, curr_cnum)
"  let at_line = search('\v\S', 'czW', 0, 0, s:skip_contained)
"  if at_line == 0
"    let result = 'ERROR-UNKNOWN'
"  else
"    let result = synIDattr(synID(line('.'), col('.'), 0), 'name')
"  endif
"  return result
"endfunction


" -------------------------------------------------------------------
" a boundary line is one that ends a backward search. there's no 
" point in looking back past a procedure statement, etc.
" -------------------------------------------------------------------
function! g:FpcIsBoundaryLine(for_lnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  return curr_lnum < 1 ||
        \ g:FpcGetFirstWord(curr_lnum) =~# s:match_boundary_word
endfunction


" -------------------------------------------------------------------
" find a hard boundary line to end backward searches. these lines
" mark the start of code units such as procedures or sections in
" a unit. returns 0 on error or not found.
"
" some class directives may need to be added here. public, private,
" static?
" -------------------------------------------------------------------
"function! g:FpcFindSearchBoundary(for_lnum)
"  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
"  if curr_lnum == 0
"    return 0
"  endif
"  let save_cursor = getcurpos()
"  call cursor(curr_lnum, 1)
"  let prior_num = search(s:match_boundary_line, 'bW', 1, 0, s:skip_contained)
"  call cursor(save_cursor[1], save_cursor[2])
"  return prior_num > 0 ? prior_num : 0
"endfunction


" -------------------------------------------------------------------
" many functions want line numbers but could be passed marks. for
" testing during development, passing raw text (eg, to FpcIs...)
" is worthwhile. this is a dwim helper. marks ., $, <, >, and a-z
" are supported.
" -------------------------------------------------------------------
" txb: wire this in
"function! g:FpcTextOrNumberFrom(arg)
"  if type(a:arg) == v:t_number
"    return a:arg
"  endif
"  if type(a:arg) != v:t_string
"    return 0
"  endif
"  return a:arg =~ '\v^(\.|\$)|(\''[a-z<>])'
"        \ ? line(a:arg)
"        \ : a:arg
"endfunction


" -------------------------------------------------------------------
" find prior code line. in addition to skipping blank and comment
" lines, any line that looks like goto labels or continued lines
" holding parts of an expression are also skipped. 
" -------------------------------------------------------------------
function! g:FpcGetPrior(for_lnum)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  if curr_lnum <= 1
    return 0
  endif
  let prev_lnum = prevnonblank(curr_lnum - 1) 
  let prev_word = g:FpcGetFirstWord(prev_lnum)
  while prev_lnum != 0 && prev_word =~# '\vfpc(Comment|String|Operator|Integer|Real|Delimiter)'
    let prev_lnum = prevnonblank(prev_lnum - 1)
    let prev_word = g:FpcGetFirstWord(prev_lnum)
  endwhile
  return prev_lnum
endfunction


" ------------------------------------------------------------------
" is this line a comment? determined by checking the highlight of
" the first non-blank on the line. 
" ------------------------------------------------------------------
function g:FpcIsComment(for_lnum)
  return g:FpcGetFirstWord(a:for_lnum) ==# 'fpcComment'
endfunction


" ------------------------------------------------------------------
" find the prior, possibly nested, word of the word pair that
" for_word closes. return its line number.
" ------------------------------------------------------------------
function g:FpcGetPriorPair(for_lnum, for_word)
  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
  if curr_lnum == 0
    return 0
  endif
  let search_from = curr_lnum

  if a:for_word ==# 'fpcEnd'
    " record/object/class is a weird case, it doesn't nest and it'd
    " require bad nesting of the pascal source to hit one after
    " processing a nested begin. treating any of record/object/class
    " as begin at this level just works, so rolling with it.
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
    echomsg printf('fpc-indent: unknown pairing word %s', a:for_word)
    return a:for_lnum - 1
  endif

  let save_cursor = getcurpos()
  call cursor(curr_lnum, 1)
  let nesting = 0
  let search_from = curr_lnum

  " prime the pump
  let where_matched = searchpair(
        \ pair_start, pair_middle, pair_end,
        \ 'bW', s:skip_contained)

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
          \ 'bW', s:skip_contained)
  endwhile

  " put cursor back once done searching
  call cursor(save_cursor[1], save_cursor[2])

  if nesting
    echomsg printf('fpc-indent: match for %s at: %d not found, nesting: %d',
          \ pair_end, curr_lnum, nesting)
    let where_matched = 0
  endif

  return where_matched
endfunction


" ------------------------------------------------------------------
" helpers to trim lines to simplify analysis. as strings can't span
" lines, remove them first. this should handle realistic nesting.
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
" i think it's more traditional to put this sort of thing in the
" ftplugin directory but let's start from here ...
"
" wrap selected lines (be in V not v) with the indent off and indent
" on directives. using the wrap function avoids a warning for a
" mismatch of directives.
"
" txb: needs better placement and scoping of code, customize
"      keybind.
" ------------------------------------------------------------------
function! g:FpcWrapIndentOffOn()
  call setreg('', '{##indent:off}' .. '\n' .. getreg('') .. '{##indent:on}' .. '\n')
endfunction
" from visual line mode, delete selection and repaste it wrapped
" in indent off/on.
vnoremap <localleader>wi x:call g:FpcWrapIndentOffOn()<enter>""P


" ------------------------------------------------------------------
" test helpers
" ------------------------------------------------------------------
"function! g:FpcIsVarOrType(for_lnum)
"  let curr_lnum = type(a:for_lnum) == v:t_string ? line(a:for_lnum) : a:for_lnum
"  if curr_lnum == 0
"    return 0
"  endif
"  let curr_line = line(curr_lnum)
"  if curr_line =~ '\v\c^\s*[a-z_][a-z0-9_]*>\s*\='
"    return 1
"  endif
"  if curr_line =~ '\v\c^\s*[a-z_][a-z0-9_]*>\s*\:[^=]'
"    return 2
"  endif
"  return 0
"endfunction

function! FpcTestMatch(seeking)
  let i = 1
  let j = 0
  echomsg printf('lines matching %s ...', a:seeking)
  while i <= line('$')
    let s = getline(i)
    if s =~ a:seeking
      echomsg printf('%d:%s', i, s)
      let j = j + 1
    endif
    let i = i + 1
  endwhile
  echomsg printf('%d lines checked, %d lines matched', (i - 1), k)
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
