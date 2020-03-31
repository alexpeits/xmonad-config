ghcid:
	nix-shell -A shell --run 'ghcid -a --command="ghci -i. -ilib/"'

watch:
	nix-shell -A shell --run 'find ~/.xmonad/ -name "*.hs" | entr -c -d xmonad --recompile'
