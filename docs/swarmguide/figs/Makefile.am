GS = @GS@
GS_LIB = @GS_LIB@
CJPEG = @CJPEG@ -quality 100
PNGTOPNM = @PNGTOPNM@
PNMTOPS = @PNMTOPS@
PNMCROP= @PNMCROP@

EPS = discrete-event-sim.eps libraries-itemized.eps swarm.eps	\
interface-vs-implementation.eps libraries-overview.eps libraries-activity.eps           modeling.eps    

GENERATED_EPS = $(patsubst %.png,%.eps,$(PNG))
PNG = 
GENERATED_JPEG = $(patsubst %.eps,%.jpeg,$(EPS)) $(patsubst %.png,%.jpeg,$(PNG))
EXTRA_DIST = swarm.eps modeling.eps interface-vs-implementation.eps discrete-event-sim.eps 

GENERATED_FIGS = $(GENERATED_EPS) $(GENERATED_JPEG)

figsdir = $(prefix)/figs
figs_DATA = $(GENERATED_FIGS) $(EPS)

CLEANFILES = $(GENERATED_FIGS)

%.png: $(srcdir)/%.eps
	GS_LIB="$(GS_LIB)" $(GS) -dNOPAUSE -sDEVICE=png256 -sOutputFile=$@ - < $< -c quit

%.jpeg: $(srcdir)/%.eps
	GS_LIB="$(GS_LIB)" $(GS) -dNOPAUSE -sDEVICE=ppm -sOutputFile=$*.ppm - < $< -c quit
	$(PNMCROP) $*.ppm > $*-crop.ppm
	$(CJPEG) $*-crop.ppm > $@
	$(RM) $*.ppm $*-crop.ppm

%.eps: $(srcdir)/%.png
	$(PNGTOPNM) $< > $*.pnm
	$(PNMTOPS) $*.pnm > $@
	$(RM) $*.pnm

%.jpeg: $(srcdir)/%.png
	$(PNGTOPNM) $< > $*.pnm
	$(CJPEG) $*.pnm > $@
	$(RM) $*.pnm
