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
RELEASEDAY	:= 2005-02-14
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

-include $(TOP)/mk/crypto.mk
include $(TOP)/mk/target.mk
