SUBDIRS = collections defobj activity \
	objectbase \
	tclobjc tkobjc gui \
	random \
	simtools simtoolsgui \
	analysis space \
	misc

EXTRA_DIST = Makefile.rules

.PHONY: headers

all-recursive: headers

headers:
	for i in $(SUBDIRS) ; do $(MAKE) -C $$i headers ; done

