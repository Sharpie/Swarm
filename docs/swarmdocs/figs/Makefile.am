GS = @GS@
CJPEG = @CJPEG@ -quality 100
PNGTOPNM = @PNGTOPNM@
PNMTOPS = @PNMTOPS@

EPS = swarm.eps
GENERATED_EPS = $(patsubst %.png,%.eps,$(PNG))
PNG = complete.png customized.png small-logo.png switcheroo.png
GENERATED_JPEG = $(patsubst %.eps,%.jpeg,$(EPS)) $(patsubst %.png,%.jpeg,$(PNG))
EXTRA_DIST = complete.png customized.png small-logo.png switcheroo.png figs.ent swarm.eps

noinst_DATA =  $(GENERATED_EPS) $(GENERATED_JPEG)

%.png: $(srcdir)/%.eps
	$(GS) -dNOPAUSE -sDEVICE=png256 -sOutputFile=$@ $< -c quit

%.jpeg: $(srcdir)/%.eps
	$(GS) -sNOPAUSE -sDEVICE=ppm -sOutputFile=$*.ppm $< -c quit
	$(CJPEG) < $*.ppm > $@
	$(RM) $*.ppm

%.eps: $(srcdir)/%.png
	$(PNGTOPNM) $< | $(PNMTOPS) > $@

%.jpeg: $(srcdir)/%.png
	$(PNGTOPNM) $< | $(CJPEG) > $@

