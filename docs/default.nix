{ sources ? import nix/sources.nix }:
let pkgs = import sources.nixpkgs { };
in
map
  (file: pkgs.texFunctions.runLaTeX {
    rootFile = file;
    # , generatePDF ? true # generate PDF, not DVI
    # , generatePS ? false # generate PS in addition to DVI
    # , extraFiles ? [ ]
    # , compressBlanksInIndex ? true
    # , packages ? [ ]
    texPackages = {
      inherit (pkgs.texlive) scheme-full;
    };
    # , copySources ? false
  })
  [
    ./tech_spec.tex
    ./project_proposal.tex
  ]
