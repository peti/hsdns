# build haddock documentation, and release archive

PACKAGE     := hsdns
RELEASE     := `date --iso-8601`
DISTARCHIVE := $(PACKAGE)-$(RELEASE).tar.gz
GHCURL      := http://haskell.org/ghc/docs/latest/html/libraries
GHCPREFIX   := /usr/local/ghc-current/share/ghc-6.3/html/libraries
CABAL       := runghc /usr/local/src/cabal-current/Setup.lhs

.PHONY: all clean distclean dist redate init-src

all::
	@$(CABAL) build
	@-mkdir docs
	@haddock -v -h -t 'HsDNS: Asynchronous DNS Resolver' \
	  -i $(GHCURL)/base,$(GHCPREFIX)/base/base.haddock \
	  -i $(GHCURL)/network,$(GHCPREFIX)/network/network.haddock \
	  -s .. -o docs */[A-Z]*.hs */*/[A-Z]*.hs

test:		test.hs
	ghc -threaded -O -Wall --make test.hs -o $@ -ladns

dist::		clean index.html

#dist::	docs/index.html index.html $(DISTFILES)
#	@rm -rf $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
#	@mkdir $(PACKAGE)-$(RELEASE)
#	@for n in $(DISTFILES); do \
#	  install -D -m 644 $$n $(PACKAGE)-$(RELEASE)/$$n; \
#	done
#	@cp -rp docs $(PACKAGE)-$(RELEASE)/
#	@echo Created $(DISTARCHIVE).
#	@tar cfvz $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
#	@rm -rf $(PACKAGE)-$(RELEASE)

index.html:	README
	@lhs2html $<
	@sed -e 's%<p>\(.*Homepage\]</a></p>\)%<p class="title">\1%' <$<.html >$@
	@rm -f README.html

clean::
	@rm -rf dist
	@rm -f `find . \( -name *.o -o -name *.hi \)`

distclean:	clean
	@rm -rf docs $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	@rm -f test README.html index.html

redate::
	@redate README *.hs* */*.hs* */*/*.hs*

init-src::
	@rm -f MT/monotonerc
	@ln -s ../.monotonerc MT/monotonerc
	@$(CABAL) configure --prefix /usr/local/ghc-current
