GS = @GS@
GS_LIB = @GS_LIB@
CJPEG = @CJPEG@ -quality 100
PNGTOPNM = @PNGTOPNM@
PNMTOPS = @PNMTOPS@
PNMCROP = @PNMCROP@

EPS = swarm.eps
GENERATED_EPS = $(patsubst %.png,%.eps,$(PNG))
PNG = complete.png customized.png small-logo.png switcheroo.png
GENERATED_JPEG = $(patsubst %.eps,%.jpeg,$(EPS)) $(patsubst %.png,%.jpeg,$(PNG))
EXTRA_DIST = complete.png customized.png small-logo.png switcheroo.png figs.ent swarm.eps

GENERATED_FIGS = $(GENERATED_EPS) $(GENERATED_JPEG)

figsdir = $(prefix)/figs
figs_DATA = $(GENERATED_FIGS) $(EPS)

CLEANFILES = $(GENERATED_FIGS)

%.png: $(srcdir)/%.eps
	GS_LIB="$(GS_LIB)" $(GS) -dNOPAUSE -sDEVICE=png256 -sOutputFile=$@ - < $< -c quit

%.jpeg: $(srcdir)/%.eps
	GS_LIB="$(GS_LIB)" $(GS) -dNOPAUSE -sDEVICE=ppm -sOutputFile=$*.ppm - < $< -c quit
	CYGWIN=binmode $(PNMCROP) $*.ppm > $*-crop.ppm
	CYGWIN=binmode $(CJPEG) $*-crop.ppm > $@
	$(RM) $*.ppm $*-crop.ppm

%.eps: $(srcdir)/%.png
	CYGWIN=binmode $(PNGTOPNM) $< > $*.pnm
	CYGWIN=binmode $(PNMTOPS) $*.pnm > $@
	$(RM) $*.pnm

%.jpeg: $(srcdir)/%.png
	CYGWIN=binmode $(PNGTOPNM) $< > $*.pnm
	CYGWIN=binmode $(CJPEG) $*.pnm > $@
	$(RM) $*.pnm

