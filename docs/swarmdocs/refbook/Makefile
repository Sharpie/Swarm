all: html tex dvi ps
html: refbook.html
tex: refbook.tex
dvi: refbook.dvi
ps: refbook.ps

include ../Makefile.conf

.PHONY: pages

pages:
	$(EMACS) -batch -l ../protocol.el -f run-all $(SWARMHOME)

