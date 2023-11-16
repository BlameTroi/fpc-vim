{ testif.pas -- small test program to check if indenting }

program testif(input, output, stderr);
const
one = 1;
two = 2;
type
str: string[32];
var
i: integer;
j: integer;
begin
for i := 1 to 100 do begin
if (i mod 3) = 0 then
writeln(i:4, ' is evenly divisible by 3 ');
    { the if below should line up under this comment }
if (i mod 6) = 0 then begin
writeln('     and evenly diviisble by 6 ');
end;
end;
end.
