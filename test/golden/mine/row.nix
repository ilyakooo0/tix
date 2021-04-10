let
  f = x: x.y + 1;
  g = x: x.y;
in
(g { y = { n = "1"; }; }).n + 1
