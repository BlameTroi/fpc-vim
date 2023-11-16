{ unnamed.txt -- how a file should be indented, a spec by example. }

(*******************************************************************)
(*                                                                 *)
(* code formatting should be viewed as a matter of local custom    *)
(* and culture. consistently formatted code is easier to read and  *)
(* quickly becomes easier to write within a team and code base.    *)
(*                                                                 *)
(* the indenting provided by the objfpc-vim plugin is indented to  *)
(* support either a k&r or allman style of placing brackets.  fans *)
(* of gnu or ratliff or other formatting will be disappointed.     *)
(*                                                                 *)
(* throughout this file, comments are interspersed with code to    *)
(* highlight indent rule examples. standard pascal has two pairs   *)
(* of comment markers: left and right curly braces and the         *)
(* digraphs 'left paren star' and 'star right paren'.              *)
(*                                                                 *)
(* as the standard does not allow comment nesting, neither does    *)
(* this plugin. the standard does perversely allow for differing   *)
(* open and close markers, so 'left curly text star right paren'   *)
(* would be allowed by a fully standard compliant compiler. this   *)
(* plugin does not.                                                *)
(*                                                                 *)
(* finally, modern compiles allow the use of the c++ style 'double *)
(* slash' to mark that all text from the slashes to the end of the *)
(* line are comments. this plugin supports them begrudgingly.      *)
(*                                                                 *)
(*******************************************************************)

{ some keywords reset indenting to the left margin. examples:

  program, uses, const, type, var, label, function, procedure,
  and the final end in a program file }

(* comments are left as found without modification with only two
  exceptions:

- compiler directives are left justified but do not reset
  indenting for subsequent lines. directives are standard
  pascal comments with the first character after the open
  comment symbol being a $.

- a comment that follows code on a line will preserve its
  spacing from the code. if the code line's indent changes,
  the whole line moves.

*)

{ the pascal standard refers to statements and compound
  statements. a compound statement is a group of one or
  more statements enclosed by the bracketing keywords
  begin and end.

  compound statements are also referred to as code blocks
  or just blocks.

  there are a few statments that can enclose multiple
  statements without requiring the begin-end bracket. this
  irregularity is annoying but is handled in this plugin
  by treating those statements as if they were additional
  compound statements.

  so, in a repeat statement 'repeat statement... until
  condition' the keywords repeat and until are treated as
  if they are synonyms for begin and end during the indent
  process.

  the irregularities are:

  - repeat-until
  - the type declaration record-end

  while not yet fully specified, tested, or supported,
  exception handling in free pascal may one or more
  irregularities. the approach will be similar to that
  used for repeat-until. }
{ the keywords end and until signal the end of a code
  block and unindent themselves and any code following
  them. }


{ *** program preamble *** }


program unnamed(input, output, stderr);

      {$mode objfpc} (* this directive will be left justified *)

uses
unit1,
          (*$ifdef DEBUG*)
debug1,
          (*$endif*)
unit2;


const
ACON = 0;


type
atyp = -1 .. 127;
btyp = record
avar: integer;
bvar: string;
end;
ctyp = array of integer;


{ *** functions with no statements that indent or outdent *** }


function afunc(arg1: integer): integer;
var
avar: integer;
bvar: string;
cvar: real;
begin
{ comments are left alone }
avar := arg1 * 3;
afunc := avar;
{ end afunc }
end;


procedure aproc(arg1: integer);
begin
writeln(afunc(arg1));
{ end aproc }
end;


{ *** functions with loops and compound statement blocks *** }


procedure bproc(arg1: integer);
var
i: integer;
x: real;
begin

{ poorly laid out for loop }
for i := 1 to arg1
do
begin
writeln(i);
end;

{ more properly laid out for loop }
for i := arg1 downto 1 do begin
writeln(i);
end;

{ repeat until }
i := 0;
repeat
wrteln(i);
i := i + 1;
until i >= arg1;

{ while do }
i := arg1;
while i > 0 do begin
writeln(i);
i := i + 1;
end;

{ end bproc }
end;


{ *** functions with if statements having compound then and else statements *** }


procedure cproc(arg1: integer; arg2: string);
var i: integer;
j: integer;
s: string;
begin

{ if then no else }
if arg1 < 10 then begin
writeln(arg2)
end;

{ if then else }
if arg1 > 9 then begin
writeln(arg2)
end else begin
writeln('nope')
end;

{ end cproc }
end;


{ begin main program }


var
{ *** demonstrate indent off/on directives for general formatting *** }
                                        {##indent:off}
      bvar: btyp;
    carr: ctyp = (1, 2, 3, 4, 5);
  i, j: integer;
    k: integer;
      x, y: real;
                                          {##indent:on}


{ *** big program block with various statement blocks *** }

{ things that aren't working right yet are wrapped by indent off/on }

begin

writeln;

for i := 1 to 200 do
begin

j := i;
k := afunc(j);

{ some if statement examples }

{##indent:off}
{ an if with a single statement }
if (i mod 5) = 0 then
writeln(i, ' is evenly divisible by 5');
writeln('this source line should align even with the if');
{##indent:on}

{##indent:off}
{ an if then else, all single statements. }
if i = 7 then
writeln('seven')
else
writeln('not seven');
{##indent:on}

{ an if without else in the k&r style }
if (i mod 4) = 0 then begin
writeln(i, ' is evenly divisible by 4');
aproc(i);
end;

{ an if without else in the allman style }
if (i mod 4) = 0 then
begin
writeln(i, ' is still evenly divisible by 4');
aproc(i);
end;

{ an if else with blocking }
if (i mod 3) = 0 then begin
writeln(i, ' is evenly divisible by 3');
end
else begin
writeln(i, ' is not evenly divisible by 3');
end;

{ end first for loop in prog block }
end;

if nargs = 1 then
begin
writeln('we need more args!');
for i := 1 to 15 do
begin
writeln(i:4);
end { for }
end { then }
else
if nargs > 5 then
begin
writeln('too many args!');
end; { if }

{ end program block }
end.
