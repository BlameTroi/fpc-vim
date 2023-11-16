{ testbed.pas -- a small clean program to get first pass working }

program testbed;
{$mode objfpc}

const
WIDTH = 64;
HEIGHT = 16;

type
mod2screen = record
colordepth: integer;
visible: boolean;
screen: array [1..HEIGHT] of array [1..WIDTH] of char;
end;
colorcell= record
r: integer;
g: integer;
b: integer;
end;

{ note one if has compound statement, outer does not }
function pset(s: mod2screen; r, c: integer): char;
begin
pset := 'E';
if (r > 0) and (r < HEIGHT) then
if (c > 0) and (c < WIDTH) then begin
pset := s.screen[r][c];
s.screen[r][c] := '*';
end;
end;

{ note both if have compount statements }
function preset(s: mod2screen; r, c: integer): char;
begin
preset := 'E';
if (r > 0) and (r < HEIGHT) then begin
if (c > 0) and (c < WIDTH) then begin
preset := s.screen[r][c];
s.screen[r][c] := ' ';
end;
end;
end;

procedure pclear(var s: mod2screen);
var
r, c: integer;
begin
s.visible := true;
s.colordepth := 16;
for r := 1 to HEIGHT do
for c := 1 to WIDTH do
s.screen[r][c] := ' ';
end;

procedure pprint(s: mod2screen);
var
r: integer;
c: integer;
begin
writeln;
r := 0;
repeat
r := r + 1;
for c := 1 to WIDTH do begin
write(s.screen[r][c]);
end;
writeln;
until r = HEIGHT;
end;

{ main vars }

var
disp: mod2screen;
i, j, k: integer;
dot: colorcell;

{ main }
begin

pclear(disp);

for i := 0 to HEIGHT do
for j := 0 to WIDTH do
begin
if (j mod 3) = 0 then
if pset(disp, i, j) = 'E' then
writeln('error! ', i, ' ', j)
end;

pprint(disp);
writeln('done');
end.
