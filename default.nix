{ pkgs ? null }:

let

  sources = import ./nix/sources.nix;
  nixpkgs =
    if builtins.typeOf pkgs == "set" then
      pkgs
    else
      import sources.nixpkgs-unstable { };

  ghcWithPackages = nixpkgs.haskellPackages.ghcWithPackages (
    self:
      with self; [
        xmonad
        xmonad-contrib
        xmonad-extras
        hostname
      ]
  );

  shell = nixpkgs.mkShell {
    shellHook = ''
      alias ghcid-orig="$(which ghcid)"
      alias ghcid="ghcid -a --command='ghci -i. -ilib/'"
    '';
    buildInputs = [ ghcWithPackages nixpkgs.haskellPackages.ghcid nixpkgs.entr ];
  };

  # testing that without -fforce-recomp for now
  script = nixpkgs.writeScriptBin "xmonad-build" ''
    #!${nixpkgs.bash}/bin/bash

    ${ghcWithPackages}/bin/ghc \
       --make xmonad.hs -i. -ilib \
       -main-is main -v0 -o "$1"
  '';

in
{
  ghc = ghcWithPackages;
  shell = shell;
  script = script;
  xmonad = nixpkgs.haskellPackages.xmonad;
}
