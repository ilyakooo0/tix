let f = x: x;
in
{
  inherit f;
  a = f 1;
}
