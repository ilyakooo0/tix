f:
{
  a = f ({ x, y }: x { c = y { }; });
  b = f ({ x, y }:
    let u = 1; in x { c = y { }; });
}
