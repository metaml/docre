.PHONY: dev undev ghc sandbox cabal shake init

PATH := /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

dev: init ghc cabal sandbox

undev:
	rm -rf ~/.cabal
	rm -rf ../../.cabal-sandbox
	rm -f ../../cabal.sandbox.config

GHC_SRC = https://www.haskell.org/ghc/dist/7.10.1/ghc-7.10.1-src.tar.xz
ghc:
	if [ ! -f /usr/local/bin/ghc ]; then \
		cd /var/tmp \
		&& wget --quiet --output-document=- ${GHC_SRC} | tar xJ \
		&& cd ./ghc-7.10.1 \
		&& ./configure --prefix=/usr/local CFLAGS=-O2 GHCFLAGS=-O2 \
		&& make -j 8 \
		&& sudo make install; \
	fi

sandbox:; cd ../.. && cabal sandbox init && cabal update && cabal install --only-dependencies

CABAL_SRC = https://www.haskell.org/cabal/release/cabal-1.22.4.0/Cabal-1.22.4.0.tar.gz
cabal:
	if [ ! -f /usr/local/bin/cabal ]; then \
		cd /var/tmp \
		&& wget --quiet --output-document=- ${CABAL_SRC} | tar xz \
		&& cd ./Cabal-1.22.4.0 \
		&& ghc --make Setup.hs \
		&& ./Setup configure --global --with-compiler=/usr/local/bin/ghc \
		&& ./Setup build \
		&& sudo ./Setup install; \
	fi

CABAL_INSTALL_SRC = https://www.haskell.org/cabal/release/cabal-install-1.22.6.0/cabal-install-1.22.6.0.tar.gz
cabal-install:
	if [ ! -f /usr/local/bin/cabal ]; then \
		cd /var/tmp \
		&& wget --quiet --output-document=- ${CABAL_INSTALL_SRC} | tar xz \
		&& cd ./cabal-install-1.22.6.0 \
		&& sudo ./bootstrap.sh --global; \
	fi

init:
	sudo apt-get update -y
	sudo apt-get install -y libgmp-dev libncurses5-dev gcc python-pygments
	sudo apt-get install -y ghc happy alex haddock dblatex xsltproc cabal-install 
