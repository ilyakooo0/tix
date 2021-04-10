let
  f = x: x.y + 1;
  g = x: x.y;
in
{
  inherit f g;
  a = f { b = ""; y = 2; };
  b = g { y = 1; } + 1;
}
