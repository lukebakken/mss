PROJECT = mss
PROJECT_DESCRIPTION = MS Storage Server
PROJECT_VERSION = 0.1.0

DEPS = cowboy lager
dep_cowboy_commit = 1.1.2

include erlang.mk

PROJDIR = $(realpath $(CURDIR))

test:
	$(PROJDIR)/mss-test
