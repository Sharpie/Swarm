NAME = refbook-java
SUBDIRS = html latex

include $(top_srcdir)/Makefile.common

MODULES = defobj collections activity objectbase random simtools gui simtoolsgui space analysis

DOC_TITLE = "Swarm $(shell head -1 $(swarm_dossrcdir)/VERSION) Reference Guide"

.PHONY: all html latex stubs

all: html latex

html: $(NAME)/index.html html/swarm-doclet.jar

latex: $(NAME).tex latex/LatexDoclet.class

java-stub.elc: $(swarm_dossrcdir)/java/java-stub.el protocol.elc
	$(elcrule)

STUBS = $(foreach module, $(MODULES),swarm/$(module)/package.html swarm/$(module))

$(STUBS): java-stub.elc $(foreach module,$(MODULES),$(swarm_dossrcdir)/src/$(module)/$(module).h)
	SWARMSRCDIR=$(swarm_dossrcdir) $(EMACS) -batch -l $(abs_builddir)/java-stub.elc -f java-run-all-javadoc

STANDARD_JAVADOC_OPTIONS = -public -sourcepath .:$(swarm_dossrcdir)/java/swarm $(foreach module, $(MODULES),swarm.$(module)) $(swarm_dossrcdir)/java/swarm/*.java

$(NAME)/index.html: $(STUBS)
	test -d $(NAME) || mkdir $(NAME)
	@JAVADOC@ $(STANDARD_JAVADOC_OPTIONS) -d $(NAME) -windowtitle $(DOC_TITLE) -doctitle $(DOC_TITLE) -docletpath $(srcdir)/html/swarm-doclet.jar -doclet Standard -link http://java.sun.com/products/jdk/1.2/docs/api

$(NAME).tex: $(STUBS)
	@JAVADOC@ $(STANDARD_JAVADOC_OPTIONS) -docletpath $(srcdir)/latex -doclet LatexDoclet -title $(DOC_TITLE) -f $(NAME).tex

MOSTLYCLEANFILES = $(NAME).tex $(NAME)/index.html protocol.elc java-stub.elc

clean-local:
	- rm -rf $(NAME) swarm
