{ testcomment.pas -- a small but valid pascal program to test comment detection in the plugin }

(* Copyright 2023 by Troy Brumley, all rights reserved. *)

{$mode objfpc} 
program testcomment;

var                    { variables here }
  i, j: integer;
  s, t: string;
  x, y: real;          { probably won't use these }

procedure counter(f, t: integer);
var
  (* this is very bad style *) i: integer;
  j: integer; // a little better
begin { lone begin }
  writeln('entering counter');
  for i := f to t do begin { for to do begin }
    j := i * 2;
    writeln(i:4, j:4);
  end; { for }
  for i := t downto f do
    writeln(i:4);
  writeln('leaving counter');
end; { procedure }

begin
  i := 7;
  { this is a multi line comment
    block
    that really doesn't say much }
  j := 14;
  s := 'test';
  t := 'testing';
  x := i / j;
  y := j / i;
  if eof then
    writeln('eof');
  writeln(x:10, y:10); { write write }
end.
