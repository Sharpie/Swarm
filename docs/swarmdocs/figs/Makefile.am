GS = @GS@
GS_LIB = @GS_LIB@
CJPEG = @CJPEG@ -quality 100
PNGTOPNM = @PNGTOPNM@
PNMTOPS = @PNMTOPS@

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
	GS_LIB="$(GS_LIB)" $(GS) -dNOPAUSE -sDEVICE=png256 -sOutputFile=$@ $< -c quit

%.jpeg: $(srcdir)/%.eps
	GS_LIB="$(GS_LIB)" $(GS) -sNOPAUSE -sDEVICE=ppm -sOutputFile=$*.ppm $< -c quit
	$(CJPEG) < $*.ppm > $@
	$(RM) $*.ppm

%.eps: $(srcdir)/%.png
	$(PNGTOPNM) $< | $(PNMTOPS) > $@

%.jpeg: $(srcdir)/%.png
	$(PNGTOPNM) $< | $(CJPEG) > $@

