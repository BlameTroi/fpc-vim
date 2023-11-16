{ in the following, the last lines pprint & writeln should
  be lined up under the for i, not the for j. by taking the
  indent of the end;, which is correct in itself, we're
  neglecting to see the additinal outdent needed. not sure
  yet how to handle this.

  gratuitous begin handles it, and that's how i would
  probably write it myself, but there must be a way to
  do this in the plugin. }

program bugnestedfor;
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

