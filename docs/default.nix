{ sources ? import nix/sources.nix }:
let
  pkgs = import sources.nixpkgs { };
  lhs2tex = file:
    let
      name = filename file;
      x = pkgs.runCommand "lhs2Tex-${name}"
        { } ''
        mkdir $out
        ${pkgs.haskellPackages.lhs2tex}/bin/lhs2TeX --output=$out/${name}.tex ${file}
      '';
    in
    "${x}/${name}.tex";
  filename = x: builtins.head (pkgs.lib.splitString "." (builtins.baseNameOf x));
in
builtins.listToAttrs (map
  (file':
    let file = if pkgs.lib.hasSuffix ".lhs" file' then lhs2tex file' else file';
    in
    {
      name = filename file';
      value = pkgs.texFunctions.runLaTeX {
        rootFile = file;
        # , generatePDF ? true # generate PDF, not DVI
        # , generatePS ? false # generate PS in addition to DVI
        extraFiles = [ (sources.TechDoc + "/TechDoc.cls") ];
        # , compressBlanksInIndex ? true
        # , packages ? [ ]
        texPackages = {
          myPackages = {
            pkgs = [
              (
                pkgs.runCommand "gost"
                  { passthru = { tlType = "run"; pname = "gost"; }; }
                  "mkdir $out; cp -r ${sources.GOST + "/bibtex"} $out"
              )
            ] ++ (builtins.filter (x: x.pname != "gost") pkgs.texlive.scheme-full.pkgs);
          };
        };
        # , copySources ? false
        searchPath = [ (pkgs.lib.cleanSource ./.) ];
      };
    })
  [
    ./tech_spec.tex
    ./project_proposal.tex
    ./paper.lhs
  ]
)
