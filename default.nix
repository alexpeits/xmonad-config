{ pkgs ? import ./nix/nixpkgs.nix {} }:

let

  ghcWithPackages = pkgs.haskellPackages.ghcWithPackages (
    self:
      with self; [
        xmonad
        xmonad-contrib
        xmonad-extras
        hostname
      ]
  );

  shell = pkgs.mkShell {
    shellHook = ''
      alias ghcid-orig="$(which ghcid)"
      alias ghcid="ghcid -a --command='ghci -i. -ilib/'"
    '';
    buildInputs = [ ghcWithPackages pkgs.haskellPackages.ghcid pkgs.entr ];
  };

  # testing that without -fforce-recomp for now
  script = pkgs.writeScriptBin "xmonad-build" ''
    #!${pkgs.bash}/bin/bash

    ${ghcWithPackages}/bin/ghc \
       --make xmonad.hs -i. -ilib \
       -main-is main -v0 -o "$1"
  '';

in
{
  ghc = ghcWithPackages;
  shell = shell;
  script = script;
  xmonad = pkgs.haskellPackages.xmonad;
}
