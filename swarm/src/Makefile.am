SUBDIRS = collections defobj activity \
	objectbase \
	tkobjc gui \
	random \
	simtools simtoolsgui \
	analysis space \
	misc

all-pre-recursive:
	for i in $(SUBDIRS) ; do make -C $$i headers ; done

