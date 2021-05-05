let
  f = x: x.a + x.b;
  g = { a, b }: a + b;
  h = { a, b, ... }: a + b;
in
{ inherit f g h; }
