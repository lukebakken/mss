PROJECT = mss
PROJECT_DESCRIPTION = MS Storage Server
PROJECT_VERSION = 0.1.0

DEPS = cowboy lager
dep_cowboy_commit = 1.1.2

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'

PROJDIR = $(realpath $(CURDIR))

test:
	$(PROJDIR)/mss-test

pkg: rel
	rm -rf $(PROJDIR)/_pkg
	mkdir -p $(PROJDIR)/_pkg/challenge/bin
	cp -f $(PROJDIR)/skel/challenge-executable $(PROJDIR)/_pkg/challenge/bin
	chmod 755 $(PROJDIR)/_pkg/challenge/bin/challenge-executable
	cp -af $(PROJDIR)/_rel/mss_release $(PROJDIR)/_pkg/challenge
	tar -C $(PROJDIR)/_pkg -czf $(PROJDIR)/mss.tgz challenge
