{ pkgs ? import ./nix/nixpkgs.nix { }, compiler ? null }:

let

  haskellPackages = if isNull compiler then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  ghcWithPackages = haskellPackages.ghcWithPackages (self: with self; [
    xmonad
    xmonad-contrib
    xmonad-extras
  ]);

  shell = pkgs.mkShell {
    shellHook = ''
      alias ghcid-orig="$(which ghcid)"
      alias ghcid="ghcid -a --command=ghci"
    '';
    buildInputs = [
      ghcWithPackages
      haskellPackages.ghcid
      pkgs.entr
    ];
  };

in

if pkgs.lib.inNixShell then shell else ghcWithPackages
