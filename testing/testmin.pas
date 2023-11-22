program a;

procedure dproc(a: integer; b: intger;
                c: real;    d: string;
                function eof: boolean);
begin
  writeln(eof);
end;

var
  i, j: integer;
begin
  i := 7;
  j := 4;
  writeln;
  if 1 < 2 then
    writeln('1 < 2');
  writeln;
  { go back to prior STATEMENT and check its first word }
  { so, find semicolon. then go back to prior semicolon }
  { and then read forward one word }

end.
