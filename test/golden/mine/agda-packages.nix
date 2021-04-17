let
  foo = self:
    let g = self.f;
    in { a = g "c"; b = g "d"; };
in
foo
