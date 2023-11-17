{ testifthenelse.pas -- testing alignment of clauses for if }
program testifthenelse;

const { just get some other text before the code }
ONE = 1;
TWO = 2;

function bool2str(b: boolean): string;
begin
bool2str := '?';
if b then
bool2str := 'true';
if not b then
bool2str := 'false';
end;

function bool2strcapital(b: boolean): string;
begin
bool2strcapital := '!';
if b
then
bool2strcapital := 'True'
else
if
not b
then
bool2strcapital := 'False';
end;

begin
writeln(bool2str(ONE = TWO));
writeln(bool2strcapital(TWO = 2));
end.
