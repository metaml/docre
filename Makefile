update:; cabal install --jobs=8 --only-dependencies

dev-tools: build
	dist/build/build shake

build:; cabal build build

init-dev:; make -f lib/dev/init-dev.make dev

