.PHONY: watch
watch:
	ghciwatch --command "cabal repl -f dev" --before-startup-shell "hpack package.yaml"

hpack:
	hpack package.yaml

ci:
	haskell-ci regenerate
	
format:
	fourmolu . -i