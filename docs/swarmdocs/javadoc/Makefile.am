NAME = refbook-java
SUBDIRS = html latex
GZIP = gzip --best

include $(top_srcdir)/Makefile.common

MODULES = defobj collections activity objectbase random simtools gui simtoolsgui space analysis

DOC_TITLE = "Swarm $(shell head -1 $(swarm_dossrcdir)/VERSION) Reference Guide"

DISTNAME=$(NAME)-@VERSION@

.PHONY: all html latex dvi ps stubs htmldist dvidist psdist

all: html latex dvi ps 

alldist: htmldist dvidist psdist

html: $(NAME)/index.html html/swarm-doclet.jar

latex: $(NAME).tex latex/LatexDoclet.class

dvi: $(NAME).dvi

ps: $(NAME).ps

htmldist: html
	tar c $(NAME) | $(GZIP) > $(DISTNAME)-html.tar.gz

dvidist: dvi
	$(GZIP) < $(NAME).dvi > $(DISTNAME).dvi.gz

psdist: ps
	$(GZIP) $(NAME).ps > $(DISTNAME).ps.gz

java-stub.elc: $(swarm_dossrcdir)/java/java-stub.el $(abs_top_builddir)/protocol.elc
	$(elcrule)

STUBS = $(foreach module, $(MODULES),swarm/$(module)/package.html swarm/$(module))

$(STUBS): java-stub.elc $(foreach module,$(MODULES),$(swarm_dossrcdir)/src/$(module)/$(module).h)
	SWARMSRCDIR=$(swarm_dossrcdir) $(EMACS) -batch -l $(abs_builddir)/java-stub.elc -f java-run-all-javadoc

STANDARD_JAVADOC_OPTIONS = -public -sourcepath .:$(swarm_dossrcdir)/java/swarm $(foreach module, $(MODULES),swarm.$(module)) $(wildcard $(swarm_dossrcdir)/java/swarm/*.java)

$(NAME)/index.html: $(STUBS) $(srcdir)/html/swarm-doclet.jar
	test -d $(NAME) || mkdir $(NAME)
	@JAVADOC@ $(STANDARD_JAVADOC_OPTIONS) -d $(NAME) -windowtitle $(DOC_TITLE) -doctitle $(DOC_TITLE) -docletpath $(srcdir)/html/swarm-doclet.jar -doclet Standard -link http://java.sun.com/products/jdk/1.2/docs/api

$(NAME).tex: $(STUBS) $(srcdir)/latex/LatexDoclet.class
	@JAVADOC@ $(STANDARD_JAVADOC_OPTIONS) -docletpath $(srcdir)/latex -doclet LatexDoclet -title $(DOC_TITLE) -nosummary -noparamtable -noclass -docauthor "Swarm Development Group" -f $(NAME).tex

$(NAME).dvi: $(NAME).tex
	- @LATEX@ $< ; @LATEX@ $< ; @LATEX@ $<

$(NAME).ps: $(NAME).dvi
	@DVIPS@ -o $@ $<

MOSTLYCLEANFILES = $(NAME)/index.html protocol.elc java-stub.elc $(addprefix $(NAME)., idx aux dvi log tex toc) 

clean-local:
	- rm -rf $(NAME) swarm
