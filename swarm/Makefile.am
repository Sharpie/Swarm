SUBDIRS = src

SUFFIXES = .m

.m.o:
	$(COMPILE) -c $< 

%.xm: %.h
	CC="$(CC)" CFLAGS="$(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)" OBJECTS="$(lib$(MODULE)_a_OBJECTS)" sh $(top_srcdir)/src/defobj/make-h2x $@ $(srcdir)

types.h classes.h: $(MODULE).h
	rm -f $(MODULE).xm; $(MAKE) $(MODULE).xm
	cp $(MODULE).xt types.h
	cp $(MODULE).xc classes.h

