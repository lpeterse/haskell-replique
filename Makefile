.PHONY: install test doc clean demo

install:
	stack install

test:
	stack test

doc:
	stack haddock

clean:
	stack clean

demo: install
	~/.local/bin/replique-demo