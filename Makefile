.PHONY: watch
watch:
	ghciwatch --command "cabal repl -f dev" --before-startup-shell "hpack package.yaml" --error-file ghcid.txt --enable-eval

.PHONY: hpack
hpack:
	hpack package.yaml

.PHONY: format
format:
	fourmolu . -i

.PHONY: clean-test
clean-test:
	rm -rf .test_builds
	rm -rf test/TestData/.hiefiles/
	rm -rf test/TestData/.hifiles/
	rm -f test/TestData/.hifiles/.hiedb

.PHONY: hlint
hlint:
	hlint -j .
