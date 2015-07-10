.PHONY: shake build clean init-dev default

default: shake

shake: update build
	bin/build shake

build: 
	cabal build build
	mkdir -p bin
	cp dist/build/build/build bin

update:; cabal install --only-dependencies

clean:;	bin/build clobber

init-dev:; make -f lib/dev/init-dev.make dev
