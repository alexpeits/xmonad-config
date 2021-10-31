ghcid:
	nix-shell -A shell --run 'ghcid -a --command="ghci -i. -ilib/"'

watch:
	nix-shell -A shell --run 'find ~/.xmonad/ -name "*.hs" | entr -c -d xmonad --recompile'

clean:
	find . -name '*.hi' -exec rm {} \;
	find . -name '*.o' -exec rm {} \;
	rm -f ./xmonad-x86_64-linux

recompile: clean
	xmonad --recompile
