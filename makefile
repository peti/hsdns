# build haddock documentation, and release archive

PACKAGE     := hsdns
RELEASE     := `date --iso-8601`
DISTARCHIVE := $(PACKAGE)-$(RELEASE).tar.gz
GHCURL      := http://haskell.org/ghc/docs/latest/html/libraries
GHCPREFIX   := /usr/local/ghc-current/share/ghc-6.3/html/libraries
GHCFLAGS    := -Wall -O -package-conf .installed-pkg-config \
               '-\#include <adns.h>' '-\#include <sys/poll.h>' \
	       '-\#include <sys/time.h>' '-\#include <errno.h>'

.PHONY: all clean dist docs

all::	docs

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

test:		test.hs
	ghc -threaded $(GHCFLAGS) --make test.hs -o $@ -ladns

docs::
	@-mkdir docs
	@haddock -h -t 'Asynchronous DNS Resolver' \
	  -i $(GHCURL)/base,$(GHCPREFIX)/base/base.haddock \
	  -i $(GHCURL)/network,$(GHCPREFIX)/network/network.haddock \
	  -s .. -o docs */[A-Z]*.hs */*/[A-Z]*.hs

index.html:	README
	@lhs2html $<
	@sed -e 's%<p>\(.*Homepage\]</a></p>\)%<p class="title">\1%' <$<.html >$@
	@rm -f README.html

distclean clean::
	rm -rf docs $(DISTARCHIVE) $(PACKAGE)-$(RELEASE)
	rm -f `find . \( -name *.o -o -name *.hi \)`
	rm -f test Network/DNS/ADNS.hs Network/DNS/ADNS_stub.?
	rm -f System/Posix/GetTimeOfDay.hs System/Posix/Poll.hs
	rm -f README.html index.html $(PACKAGE)-*.tar.gz

redate::
	redate $(DISTFILES)

init-src::
	@rm -f MT/monotonerc
	@ln -s ../.monotonerc MT/monotonerc
