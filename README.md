# xmonad-config

## Requirements

- the `xmonad` executable in `$PATH`
- the `xmonad-build` script

## Installation

1. Install the `xmonad` executable

```
$ nix-env -i -f default.nix -A xmonad
```

Alternatively:

```
$ nix-build -A xmonad
```

And then copy the `xmonad` executable from `result/bin/xmonad` somewhere
in the `$PATH`. This is fragile because in case of `nix-collect-garbage`
some symlinks might disappear and the executable will stop working.

2. Install the `xmonad-build` script

```
$ nix-env -i -f default.nix -A script
```

Alternatively:

```
$ nix-build -A script
```

Then either copy the `xmonad-build` executable somewhere in
`$PATH` or leave it there. Same warning as the previous section about
`nix-collect-garbage`.

## Usage

XMonad will automatically use the `build` script located
in `~/.xmonad` to build itself. To test if everything
works:

```
$ xmonad --recompile
```

This should exit successfully

## Development

A nix-shell environment with everything useful is provided:

```
$ nix-shell -A shell
```

Thanks to the `.ghci` file and the `ghcid` alias provided in
nix-shell, just running `ghcid` should load `xmonad.hs` and
every module under `lib/` (that's specified in `.ghcid`).

To customize the `ghcid` command, the original `ghcid` executable
is aliased to `ghcid-orig`.

To test recompilation, `entr` is also provided:

```
$ find ~/.xmonad/ -name "*.hs" | entr -c -d xmonad --recompile
```
