NAMES = AbstractIndexWriter AbstractPackageIndexWriter			\
AbstractPackageWriter AbstractSubWriter AbstractTreeWriter		\
AllClassesFrameWriter ClassSubWriter ClassUseMapper ClassUseWriter	\
ClassWriter ConfigurationStandard ConstructorSubWriter			\
DeprecatedAPIListBuilder DeprecatedListWriter				\
ExecutableMemberSubWriter Extern FieldSubWriter FrameOutputWriter	\
Group HelpWriter HtmlStandardWriter MethodSubWriter PackageFrameWriter	\
PackageIndexFrameWriter PackageIndexWriter PackageListWriter		\
PackageTreeWriter PackageUseWriter PackageWriter PackagesFileWriter	\
SerialFieldSubWriter SerialMethodSubWriter SerializedFormWriter		\
SingleIndexWriter SplitIndexWriter Standard StylesheetWriter		\
SubWriterHolderWriter TreeWriter

JAVASOURCE = $(addsuffix .java, $(NAMES))
CLASSES = $(addsuffix .class, $(NAMES))

EXTRA_DIST = swarm-doclet.jar $(JAVASOURCE)

if MAINTAINER_MODE
all: $(srcdir)/swarm-doclet.jar
$(CLASSES): $(addprefix $(srcdir)/, $(JAVASOURCE))
	javac -d . $(addprefix $(srcdir)/, $(JAVASOURCE))
$(srcdir)/swarm-doclet.jar: $(CLASSES)
	jar cvf $(srcdir)/swarm-doclet.jar $(CLASSES)
clean-local:
	- rm -f $(srcdir)/swarm-doclet.jar $(wildcard *.class)
endif