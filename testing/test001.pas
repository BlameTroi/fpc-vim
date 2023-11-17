{ test001 }

{$mode objfpc}

program test001;

(* single line comment *)
{ another single line comment }
(* multi
   line
   comment
*)
{##indent:off}
{##indent:on}

#define EOFVAL -1

const
END_FILE = EOFVAL;
SPACE = 32;
NULL = 0;
MAXSTR = 255;

{ multi

  line

  comment }

type
vstring = record
l: 0 .. MAXSTR;
s: packed array[1 .. MAXSTR] of char;
end;
alignwithend = -1 .. 1;

procedure if001(i, j: integer);
begin
if i < j then
writeln(i);
writeln('should outdent');
end;

procedure if002(i, j: integer);
begin
if i < j
then
writeln(i);
writeln('should outdent');
end;

procedure if003(i, j: integer);
begin
if i < j
then writeln(i);
writeln('should outdent');
end;

{ there is only one variety of repeat layout }

procedure repeat001(i, j: integer);
begin
repeat
writeln(i);
i := i + 1;
until i > j;
writeln('align with until');
end;

{ there are three varieties of while layouts }

procedure while001(i, j: integer);
begin
while i <= j do begin
writeln(i);
i := i + 1;
end;
writeln('align with end');
end;

procedure while002(i, j: integer);
begin
while i <= j do
begin
writeln(i);
i := i + 1;
end;
writeln('align with end');
end;

procedure while003(i, j: integer);
begin
while i <= j
do
begin
writeln(i);
i := i + 1;
end;
writeln('align with end');
end;

{ a main program }
var
i: integer;
x, y: real;
v: vstring;

begin

for i := 1 to 3 do begin
writeln('***');
end;

v.i := 0;
for i := 1 to MAXSTR
do begin
v.s[i] := chr(SPACE)
end;

for i := 1 to 3
do
begin
writeln('***');
end;

with v do begin
l := 1;
s[1] := 's';
end;

with v
do begin
l := 2;
s[2] := 't';
end;

with v
do
begin
l := 3;
s[3] := 't';
end;

if v.l = 1 then begin
writeln('1');
end;

if v.l = 2 then
writeln('2');

end.
