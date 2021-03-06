{ sources ? import nix/sources.nix }:
let
  pkgs = import sources.nixpkgs { };
  lhs2tex = file:
    let
      name = filename file;
      x = pkgs.runCommand "lhs2Tex-${name}"
        { } ''
        mkdir $out
        ${pkgs.haskellPackages.lhs2tex}/bin/lhs2TeX --output="$out/${name}.tex" ${file}
      '';
    in
    "${x}/${name}.tex";
  filename = x: builtins.head (pkgs.lib.splitString "." (builtins.baseNameOf x));
in
builtins.listToAttrs (map
  (file':
    let file = if pkgs.lib.hasSuffix ".lhs" file'.file then lhs2tex file'.file else file'.file;
    in
    {
      name = filename file'.file;
      value = pkgs.texFunctions.runLaTeX {
        rootFile = file;
        # , generatePDF ? true # generate PDF, not DVI
        # , generatePS ? false # generate PS in addition to DVI
        extraFiles = file'.extraFiles or [ ];
        # , compressBlanksInIndex ? true
        # , packages ? [ ]
        texPackages = {
          inherit (pkgs.texlive) scheme-medium IEEEtran polytable lazylist multirow;
        };
        # , copySources ? false
      };
    })
  [
    { file = ./tech_spec.tex; }
    { file = ./project_proposal.tex; }
    { file = ./paper.lhs; extraFiles = [ ./bibl.bib ./trfrac.sty ]; }
  ]
)
