let f = { a, b, ... }: a + b;
in { inherit f; a = f { a = 1; b = 2; }; b = f { a = 1; b = 2; c = 3; }; }
