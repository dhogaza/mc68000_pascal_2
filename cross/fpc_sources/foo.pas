program foo;
type
  r = record a,b: integer end;
  p = ^r;

var v: r;
    pp: p;
begin
  v.a := 3; v.b := 4;
  pp := @(v);
  writeln(pp^.a);
end.
