SAMPLE_FILES = run.js rungui.js test.js testgui.js test.html testgui.html
EXTRA_DIST = COM.el move-if-change c/Makefile.in c/COMsupport.h c/COMsupport.cpp $(SAMPLE_FILES)
EMACS = @EMACS@

 include $(top_srcdir)/etc/Makefile.protocol


interface.elc: $(abs_top_builddir)/protocol.elc

COM.elc: $(abs_top_builddir)/protocol.elc $(abs_top_builddir)/interface.elc

generate: COM.elc findImp
	$(ENVEMACS) -batch -l '$(dos_abs_builddir)@PATHDELIM@COM.elc' -f run
	$(srcdir)/move-if-change new-uuids.el $(srcdir)/uuids.el

findImp: findImp.m
	../libtool --mode=link gcc -D_GNU_SOURCE -DAPPNAME=findImp -o findImp -Wall -Werror -g -Wno-import -I$(top_builddir) -I$(top_builddir)/src -I$(top_srcdir)/src -I$(top_srcdir)/src/defobj -I$(top_srcdir)/src/collections -I$(top_srcdir)/src/activity -I$(top_srcdir)/src/objectbase -I$(top_srcdir)/src/random -I$(top_srcdir)/src/simtools -I$(top_srcdir)/src/simtoolsgui -I$(top_srcdir)/src/misc -I$(top_srcdir)/src/gui -L../libobjc -L../src $< -lswarm -lobjc

