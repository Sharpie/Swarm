SUBDIRS = collections defobj activity \
	objectbase \
	tkobjc gui \
	random \
	simtools simtoolsgui \
	analysis space \
	misc

.PHONY: headers

all-recursive: headers

headers:
	for i in $(SUBDIRS) ; do make -C $$i headers ; done

