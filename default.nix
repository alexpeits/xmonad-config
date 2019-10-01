{ pkgs ? import ./nix/nixpkgs.nix { }, compiler ? null }:

let

  haskellPackages = if isNull compiler then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  ghcWithPackages = haskellPackages.ghcWithPackages
    (self: with self; [ xmonad xmonad-contrib xmonad-extras ]);

  shell = pkgs.mkShell {
    shellHook = ''
      alias ghcid-orig="$(which ghcid)"
      alias ghcid="ghcid -a --command='ghci -i. -ilib/'"
    '';
    buildInputs = [ ghcWithPackages haskellPackages.ghcid pkgs.entr ];
  };

  # testing that without -fforce-recomp for now
  script = pkgs.writeScriptBin "xmonad-build" ''
    #!${pkgs.bash}/bin/bash
    base="$HOME/.xmonad"
    extra_flags_filename="extra-ghc-flags"

    hostname="$(hostname)"
    host_dir="$base/hosts/$hostname"
    extra_flags_file="$host_dir/$extra_flags_filename"

    if [ -f "$extra_flags_file" ]; then
        extra_flags="$(cat "$extra_flags_file" | tr '\n' ' ')"
        echo "Recompiling xmonad with flags: $extra_flags"
    else
        extra_flags=""
        echo "Recompiling xmonad (no flags)"
    fi

    ${ghcWithPackages}/bin/ghc \
       --make xmonad.hs -i. -ilib \
       -main-is main -v0 -o "$1" $extra_flags
  '';

in {
  ghc = ghcWithPackages;
  shell = shell;
  script = script;
  xmonad = haskellPackages.xmonad;
}
