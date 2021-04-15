g:
let packages = self:
  let
    f = g self;
    prefer = version: path:
      let packages = f path { }; in
      packages."${version}".overrideAttrs (_: { passthru = packages; });
    foo = f "b" { };
  in
  {
    Prelude =
      prefer "d" "f";
  };
in packages
