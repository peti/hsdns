# build haddock documentation, and release archive

PACKAGE		:= hsdns
RELEASE		:= `date --iso-8601`
DISTNAME	:= $(PACKAGE)-$(RELEASE)
GHCURL		:= http://haskell.org/ghc/docs/latest/html/libraries
GHCPREFIX	:= /usr/local/ghc-current/share/ghc-6.3/html/libraries
CABAL		:= runghc /usr/local/src/cabal-current/Setup.lhs

.PHONY: all docs clean distclean dist redate init-src

all::
	@$(CABAL) build

test:		test.hs
	ghc -threaded -O -Wall --make test.hs -o $@ -ladns

docs::
	@-mkdir docs
	@haddock -v -h -t 'An asynchronous DNS resolver' \
	  -i $(GHCURL)/base,$(GHCPREFIX)/base/base.haddock \
	  -i $(GHCURL)/network,$(GHCPREFIX)/network/network.haddock \
	  -s .. -o docs */[A-Z]*.hs */*/[A-Z]*.hs

dist::		docs clean index.html
	@darcs dist --dist-name $(DISTNAME)

index.html:	README
	@lhs2html $<
	@sed -e 's%<p>\(.*Homepage\]</a></p>\)%<p class="title">\1%' <$<.html >$@
	@rm -f README.html

clean::
	@rm -rf dist
	@rm -f test README.html `find . \( -name *.o -o -name *.hi \)`

distclean:	clean
	@rm -rf docs $(DISTNAME).tar.gz
	@rm -f index.html System/Posix/GetTimeOfDay.hs
	@rm -f System/Posix/Poll.hs Network/DNS/ADNS.hs
	@rm -f .setup-config .installed-pkg-config

redate::
	@redate README $(PACKAGE).cabal

init-src::
	@rm -f MT/monotonerc
	@ln -s ../.monotonerc MT/monotonerc
	@$(CABAL) configure --prefix /usr/local/ghc-current
