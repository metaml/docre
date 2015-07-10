.PHONY: shake build clean init-dev

shake: build
	bin/build shake

build: 
	cabal build build
	mkdir -p bin
	cp dist/build/build/build bin

clean:; ./bin/build clean

init-dev:; make -f lib/dev/init-dev.make dev
