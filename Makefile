.PHONY: watch
watch:
	ghciwatch --command "cabal repl -f dev" --before-startup-shell "hpack package.yaml" --error-file errors.txt

hpack:
	hpack package.yaml

ci:
	haskell-ci regenerate --distribution jammy
	
format:
	fourmolu . -i

clean:
	rm -r .test_builds