{ fetchurl }:
let

  fetchNuGet = { url }: fetchurl {
    inherit url;
  };

in
[
  (fetchNuGet {
    url = "https://www.nuget.org/api/v2/package/microsoft.build/14.3.0";
  })

]
