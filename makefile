# build haddock documentation, and release archive

PACKAGE     := hsdns
RELEASE     := `date --iso-8601`
DISTARCHIVE := $(PACKAGE)-$(RELEASE).tar.gz
DISTFILES   := ADNS.hsc PollResolver.hs test.hs README
GHCURL      := http://haskell.org/ghc/docs/latest/html/libraries
GHCPREFIX   := /usr/local/ghc-current/share/ghc-6.3/html/libraries
GHCFLAGS    := -Wall -O \
               '-\#include <adns.h>' '-\#include <sys/poll.h>'

.PHONY: all clean dist

all::	docs/index.html

dist::	docs/index.html index.html $(DISTFILES)
	@rm -rf $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	@mkdir $(PACKAGE)-$(RELEASE)
	@cp -rp $(DISTFILES) docs $(PACKAGE)-$(RELEASE)/
	@echo Created $(DISTARCHIVE).
	@tar cfvz $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	@rm -rf $(PACKAGE)-$(RELEASE)

ADNS.hs:	ADNS.hsc
	hsc2hs $<


test:		ADNS.hs PollResolver.hs test.hs
	ghc -threaded $(GHCFLAGS) --make test.hs -o $@ -ladns

docs/index.html: ADNS.hs $(DISTFILES)
	@-mkdir docs
	@hsc2hs ADNS.hsc
	@haddock -h -t 'Asynchronous DNS Resolver' \
	  -i $(GHCURL)/base,$(GHCPREFIX)/base/base.haddock \
	  -i $(GHCURL)/network,$(GHCPREFIX)/network/network.haddock \
	  -s .. -o docs [A-Z]*.hs

index.html:	README
	@lhs2html $<
	@sed -e 's%<p>\(.*Homepage\]</a></p>\)%<p class="title">\1%' <$<.html >$@
	@rm -f README.html

distclean clean::
	rm -rf docs
	rm -f *.o *.hi a.out test ADNS.hs ADNS_stub.?
	rm -f README.html $(PACKAGE)-*.tar.gz

redate::
	redate $(DISTFILES)

init-src::
	@rm -f MT/monotonerc
	@ln -s ../.monotonerc MT/monotonerc
