GRIDDIR=grid
EXAMPLES=GridTurtle.h GridTurtle.m Makefile Mousetrap.h Mousetrap.m \
	Mousetrap2.m grid.h grid.m grid0.m grid1a.m grid1b.m grid1c.m\
	grid2.m grid2b.m grid3.m grid3b.m grid4.m  grid4b.m grid5.m \
	grid6.m grid7.m grid8.m grid9.m mousetraps.m mousetraps2.m \
	strtest.m
LIBS=defobj collections activity objectbase random simtools simtoolsgui \
	gui analysis
LIBPAGES=$(addsuffix pages.sgml,$(join $(addsuffix /,$(LIBS)),$(LIBS)))
MAKEPAGES=$(EMACS) -batch -l ../protocol.el -f run-all $(SWARMHOME)

include ../Makefile.conf

all: html tex dvi ps rtf
html: refbook.html
tex: refbook.tex
dvi: refbook.dvi
ps: refbook.ps
rtf: refbook.rtf

refbook.html refbook.tex refbook.dvi refbook.ps refbook.rtf: $(GRIDDIR)/gridexamples.sgml $(LIBPAGES)

$(LIBPAGES): 
	$(MAKEPAGES)

$(GRIDDIR)/gridexamples.sgml: $(addprefix $(GRIDDIR)/,$(EXAMPLES))
	- (cd $(GRIDDIR) ; (for i in $(EXAMPLES); do echo "<EXAMPLE ID=\"GRID.$$i\">"; echo "<TITLE>$$i</TITLE>"; echo "<PROGRAMLISTING>"; echo '<![ CDATA [' ; cat $$i ; echo ']]>'; echo "</PROGRAMLISTING>"; echo "</EXAMPLE>";  done) > gridexamples.sgml; cd ..)

# targets to force remake of examples and reference entries

.PHONY: pages examples

examples:  $(GRIDDIR)/gridexamples.sgml

pages:  
	$(MAKEPAGES)

realclean: clean
	rm -f $(GRIDDIR)/gridexamples.sgml
	rm -f $(LIBPAGES)

