EPS = swarm.eps
GENERATED_EPS = $(patsubst %.png,%.eps,$(PNG))
PNG = complete.png customized.png small-logo.png switcheroo.png
GENERATED_JPEG = $(patsubst %.eps,%.jpeg,$(EPS)) $(patsubst %.png,%.jpeg,$(PNG))
CJPEG = cjpeg -quality 100

noinst_DATA =  $(GENERATED_EPS) $(GENERATED_JPEG)

%.png: $(srcdir)/%.eps
	gs -dNOPAUSE -sDEVICE=png256 -sOutputFile=$@ $< -c quit

%.jpeg: $(srcdir)/%.eps
	gs -sNOPAUSE -sDEVICE=ppm -sOutputFile=$*.ppm $< -c quit
	$(CJPEG) < $*.ppm > $@
	$(RM) $*.ppm

%.eps: $(srcdir)/%.png
	pngtopnm $< | pnmtops > $@

%.jpeg: $(srcdir)/%.png
	pngtopnm $< | $(CJPEG) > $@

