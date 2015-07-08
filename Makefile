build:; cabal build build

devtools: build
	dist/build/build shake
