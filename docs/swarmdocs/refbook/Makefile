GRIDDIR=grid
EXAMPLES=GridTurtle.h GridTurtle.m Makefile Mousetrap.h Mousetrap.m \
	Mousetrap2.m grid.h grid.m grid0.m grid1a.m grid1b.m grid1c.m\
	grid2.m grid2b.m grid3.m grid3b.m grid4.m  grid4b.m grid5.m \
	grid6.m grid7.m grid8.m grid9.m mousetraps.m mousetraps2.m \
	strtest.m

all: html tex dvi ps rtf
html: refbook.html
tex: refbook.tex
dvi: refbook.dvi
ps: refbook.ps
rtf: refbook.rtf

include ../Makefile.conf

.PHONY: pages egs

egs:  $(GRIDDIR)/gridexamples.sgml

pages:  
	$(EMACS) -batch -l ../protocol.el -f run-all $(SWARMHOME)

$(GRIDDIR)/gridexamples.sgml: $(addprefix $(GRIDDIR)/,$(EXAMPLES))
	- (cd $(GRIDDIR) ; (for i in $(EXAMPLES); do echo "<EXAMPLE ID=\"GRID.$$i\">"; echo "<TITLE>$$i</TITLE>"; echo "<PROGRAMLISTING>"; echo '<![ CDATA [' ; cat $$i ; echo ']]>'; echo "</PROGRAMLISTING>"; echo "</EXAMPLE>";  done) > gridexamples.sgml; cd ..)

