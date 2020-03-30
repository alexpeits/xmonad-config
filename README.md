# xmonad-config

## Requirements

- the `xmonad` executable in `$PATH`
- the `xmonad-build` script

## Installation

1. Install the `xmonad` executable

```
$ nix-env -i -f default.nix -A xmonad
```

2. Install the `xmonad-build` script

```
$ nix-env -i -f default.nix -A script
```

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
