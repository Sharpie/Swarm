EXTRA_DIST = COM.el move-if-change c/Makefile.in c/COMsupport.h c/COMsupport.cpp c/swarmITyping.idl c/swarmIBase.idl c/swarmBase.h c/swarmBase.cpp c/swarmSelectorImpl.h c/swarmSelectorImpl.cpp
EMACS = @EMACS@

 include $(top_srcdir)/etc/Makefile.protocol

interface.elc: $(abs_top_builddir)/protocol.elc

COM.elc: $(abs_top_builddir)/protocol.elc $(abs_top_builddir)/interface.elc

generate: COM.elc
	$(ENVEMACS) -batch $(GUIOPT) -l '$(dos_abs_builddir)@PATHDELIM@COM.elc' -f run
	$(srcdir)/move-if-change new-uuids.el $(srcdir)/uuids.el

