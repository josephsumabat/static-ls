.PHONY: watch
watch:
	ghciwatch --command "cabal repl -f dev" --before-startup-shell "hpack package.yaml" --error-file ghcid.txt --enable-eval

.PHONY: hpack
hpack:
	hpack package.yaml

.PHONY: format
format:
	fourmolu . -i

.PHONY: clean
clean:
	rm -r .test_builds

.PHONY: hlint
hlint:
	hlint -j .