{ mkDerivation
, a
, b
, c
, g
}:
{
  libraryHaskellDepends = [ a b ];
  librarySystemDepends = [ c g ];
}
