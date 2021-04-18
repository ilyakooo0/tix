let attr = { a = 1; b = "hello"; };
in
with attr; rec {
  f = x: x + a;
  foo = f 8;
}
