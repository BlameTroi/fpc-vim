{ testsmall.pas -- a small program }
program testsmall(input, output);
{$mode objfpc}
(*$I-*)
{##indent:off}
#define EOF -1
{##indent:on}

{   some
      constants
        for
      testing
    boundaries  }

const
  ENDSTR = 0;
  CR = 13;
  NL = 10;

{ various types }
type
  scharacter = -1 .. 127;
  sstring = packed array [1..255] of character;

    {
mutliline
    }

var
  i: integer;
{ multi
  line }
  c: scharacter;
        (* single line *)
  s: sstring;

begin
  writeln('test');
(* some text *)
  for i := 32 to 127 do { ascii printables }
  begin { standing alone }
    c := chr(i);
    if (i div 8 = 0) and (i > 32) then begin { standing behind }
      writeln;
    end;
    write(c);
  end;
  writeln('done');
end.
