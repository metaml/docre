.PHONY: shake build clean init-dev default

default: shake

shake: update build
	bin/build shake

build: 
	cabal build build
	mkdir -p bin
	cp dist/build/build/build bin

update:; cabal install --only-dependencies

init-dev: clobber
	cd lib/dev && make -f init-dev.make dev

clean:;	if [ -f bin/build ]; then bin/build clobber; fi

clobber: clean
	rm -rf ~/.cabal ~/.ghc .cabal-sandbox
