XSLPARAMS = --param section.autolabel 1 --param shade.verbatim 1

all: $(addprefix README., html pdf ps) index.html README

%: %.html
	lynx -dump -nolist $< > $@

%.html: %.xml
	xsltproc $(XSLPARAMS) -o $@ $(DOCBOOKXSLDIR)/html/docbook.xsl $< 

index.html: README.xml
	xsltproc $(XSLPARAMS) -o $@ $(DOCBOOKXSLDIR)/html/chunk.xsl $< 

%.fo: %.xml
	xsltproc $(XSLPARAMS) -o $@ $(DOCBOOKXSLDIR)/fo/docbook.xsl $<

%.pdf: %.fo
	Fop $< $@

%.ps: %.fo
	Fop $< -ps $@

clean:
	- rm -f $(addprefix README., ps pdf fo) *.html README
