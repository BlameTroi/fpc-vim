program testunit;
{ a comment }

procedure test0000(a: integer; b: string);
var
  i: integer;
begin
  for i := 1 to a do
  begin
    { and another comment }
    writeln(b);
  end;
end;

begin
  test0000;
end.
