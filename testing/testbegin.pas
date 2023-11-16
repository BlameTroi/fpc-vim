{ testbegin.pas -- small test program to check begin indenting }
{$mode objfpc}

program testbegin(input, output, stderr);
var
i: integer;
j: integer;
begin
for i := 1 to 100 do
begin
if (i mod 3) = 0 then
begin
writeln(i:4, ' is evenly divisible by 3 ');
if (i mod 6) = 0 then begin
writeln('     and evenly diviisble by 6');
end else begin
writeln;
end;
end;
end;
end.
