let f = x: x.y;
in
{
  inherit f;
  out = f { y = 1; };
}
