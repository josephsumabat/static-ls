.PHONY: watch
watch:
	ghciwatch --command "cabal repl -f dev" --before-startup-shell "hpack package.yaml"

hpack:
	hpack package.yaml
	haskell-ci regenerate