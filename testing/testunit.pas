program testunit;
{ a comment }

type
  somerecord = record
    i: integer;
    s: string;
  end;
  psomerecord = ^somerecord;

var
  sr: psomerecord;

function test0002(x: integer;
                  t: string): psomerecord;
var
  nsr: psomerecord;
begin
  new(nsr);
  with nsr^ do
    i := x;
  nsr^.s := t;
  test0002 := nsr;
end;

{ test allman style begin end under for }
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

{ test align after single statement for }
procedure test0001(a: integer; b: string);
var
  i: integer;
begin
  for i := 1 to a do
    write(b);
  writeln;
  { the above writeln should outdent to be even with for }
end;

begin
  test0000(15, '*');
  test0001(32, 'F');
  sr := test0002(32, 'asdf');
end.
