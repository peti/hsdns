# build haddock documentation, and release archive

PACKAGE     := hsdns
RELEASE     := `date --iso-8601`
DISTARCHIVE := $(PACKAGE)-$(RELEASE).tar.gz
DISTFILES   := Data/Endian.hs Network/DNS/ADNS.hsc Network/DNS/PollResolver.hs \
               Network/DNS.hs test.hs README hsdns.cabal
GHCURL      := http://haskell.org/ghc/docs/latest/html/libraries
GHCPREFIX   := /usr/local/ghc-current/share/ghc-6.3/html/libraries
GHCFLAGS    := -Wall -O \
               '-\#include <adns.h>' '-\#include <sys/poll.h>'

.PHONY: all clean dist

all::	docs/index.html

dist::	docs/index.html index.html $(DISTFILES)
	@rm -rf $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	@mkdir $(PACKAGE)-$(RELEASE)
	for n in $(DISTFILES); do \
	  install -D -m 644 $$n $(PACKAGE)-$(RELEASE)/$$n; \
	done
	@cp -rp docs $(PACKAGE)-$(RELEASE)/
	@echo Created $(DISTARCHIVE).
	@tar cfvz $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	@rm -rf $(PACKAGE)-$(RELEASE)

%.hs:	%.hsc
	hsc2hs -o $@ $<

test:		Network/DNS/ADNS.hs Network/DNS/PollResolver.hs test.hs
	ghc -threaded $(GHCFLAGS) --make test.hs -o $@ -ladns

docs/index.html: Network/DNS/ADNS.hs $(DISTFILES)
	@-mkdir docs
	@haddock -h -t 'Asynchronous DNS Resolver' \
	  -i $(GHCURL)/base,$(GHCPREFIX)/base/base.haddock \
	  -i $(GHCURL)/network,$(GHCPREFIX)/network/network.haddock \
	  -s .. -o docs `find . -name [A-Z]*.hs`

index.html:	README
	@lhs2html $<
	@sed -e 's%<p>\(.*Homepage\]</a></p>\)%<p class="title">\1%' <$<.html >$@
	@rm -f README.html

distclean clean::
	rm -rf docs $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	rm -f `find . \( -name *.o -o -name *.hi \)`
	rm -f test Network/DNS/ADNS.hs Network/DNS/ADNS_stub.?
	rm -f README.html index.html $(PACKAGE)-*.tar.gz

redate::
	redate $(DISTFILES)

init-src::
	@rm -f MT/monotonerc
	@ln -s ../.monotonerc MT/monotonerc
