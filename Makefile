TOP=..
include $(TOP)/mk/boilerplate.mk

# ---------------------------------------------------------------

ALL_DIRS =	       \
    Data	       \
    Network	       \
    Network/DNS	       \
    Network/IP	       \
    System/Posix

PACKAGE		:= hsdns
RELEASEDAY	:= 2005-02-10
VERSION		:= 0.0-$(RELEASEDAY)
PACKAGE_DEPS	:= base network

SRC_HSC2HS_OPTS += $(hsdns_SRC_HSC2HS_OPTS)
SRC_HC_OPTS	+= -Wall -fffi			\
		   '-\#include <adns.h>'	\
		   '-\#include <sys/poll.h>'	\
		   '-\#include <sys/time.h>'	\
		   '-\#include <errno.h>'	\
		   $(hsdns_SRC_HSC2HS_OPTS)

SRC_HADDOCK_OPTS += -t "Asynchronous DNS Resolver ($(PACKAGE) package)"

# ---------------------------------------------------------------

dist::
	@darcs dist --dist-name $(PACKAGE)-$(RELEASEDAY)

redate::
	@redate Makefile $(PACKAGE).cabal package.conf.in

init-src::
	@rm -f MT/monotonerc
	@ln -s ../.monotonerc MT/monotonerc

# ---------------------------------------------------------------

include $(TOP)/mk/target.mk
