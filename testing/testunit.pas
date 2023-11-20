program testunit;

procedure test0000(a: integer; b: string);
var
  i: integer;
begin
  for i := 1 to a do
  begin
    writeln(b);
  end;
end;

begin
  test0000;
end.
