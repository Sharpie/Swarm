import com.sun.javadoc.*;
import java.util.*;
import java.text.*;
import java.io.*;

/**
 * This class is a doclet that generates Javadoc help as a LaTex file.  LaTex is
 * a macro package for Donald Knuth's excellent Tex type-setting package.  It
 * is available on virtually every platform.  This doclet generates code that
 * conforms to the LaTex 2e distribution.  It requires one non-standard package,
 * the fancyhdr package.
 * <p>
 * Javadoc documentation is frequently enhanced by adding html tagging to the
 * comments.  Obviously, this will not do anything useful in a LaTex file, so
 * this package attempts to make a reasonable translation for most of the
 * tags commonly found in Sun's documentation.  Fortunately, it is fairly easy
 * to map these tags into LaTex, but it is not always perfect.
 * <p>
 * <p>
 * This doclet adds the following command line options to javadoc:
 * <dl>
 * <dt>-f <file>
 * <dd>Specify the output filename.  Defaults to javadoc.tex
 * <dt>-title "title"
 * <dd>Specify the title for the documentation File.  This will be displayed on
 *     the title page
 * <dt>-docauthor
 * <dd>Specify the name of the document author.  This will be displayed on
 *     the title page.
 * <dt>-twoside
 * <dd>Specifies that the document will eventually be printed double-sided.  Tbe
 *     output will be formatted appropriately for this.
 * <dt>-nodetails
 * <dd>The detailed constructor, method, and field information will not be shown.
 *     Only the summaries will be shown.  This results in a good reference type document.
 * <dt>-nosummary
 * <dd>The summaries will be omitted, but method, constructor, and field details will
 *     be shown.  This results in a slightly shorter document.
 * </dl>
*/
public class LatexDoclet {
/** The output is written to this Writer. */
    static private PrintWriter out = null;
/** This variable is incremented every time a heading id is specified, so that each
    heading id will be unique.
*/
    static private int ref = 1;
/** The name of the output file.  This defaults to javadoc.tex, but can be overridden
    by the -f command line option.
*/
    static private String outputFileName="javadoc.tex";
/** The title to use for the latex file.  Defaults to "Javadoc Documentation" */
    static private String title="Javadoc Documentation";
/** The document author - used on the title page.  Defaults to an empty string */
    static private String author = "";
/** Flag indicates whether to show the details of constructors, methods, and fields. */
    static private boolean showDetails = true;
/** Falg indicates whether to show the summaries of constructors, methods, and fields. */
    static private boolean showSummary = true;
/** Flag indicating whether document should be formatted for double-sided printing or not. */
    static private boolean twoSide = false;

/** don't generate the parameter table for methods */
    static private boolean paramTable = true;

/** if false, don't any class info except for name, and interface conformance */
    static private boolean showClass = true;

/** This flag is used when converting definition lists from html to latex.  On the first line,
   we do not start the line with \\.  On other lines we do.
*/
    static private boolean definitionListFirstLine = false;
/** An array of all the package names documented in the IPF being generated. */
    static private String[] ourPackages = null;
/** This hashtable is used to match heading ids to fully qualified class names.  We use these to look
    up the ids for @see tags and other links internal to the generated ipf file.
*/
    static private Hashtable classIds = null;
/** Holds the link id of the package currently being processed */
    static private int currentPackageId = 1;
/** Holds the id of the panel with the list of all packages */
    static private int packageListId = 1;
/** Holds the id of the panel listing all classes in the generated file */
    static private int allClassesRef = 1;
/** This is the RootDoc object passed to the start method.  We keep a static reference to it
    because it has the DocErrorReporter in it that we may need to use.
*/
    static private RootDoc doc=null;

/**
 * Called to start processing documentation.  This is the entry point for the doclet, and
 * it handles outputting all the preliminary pages such as the package list,
 * and sets up the class ids.  It then processes each package one by one by calling
 * <code>processPackage</code>.
 * @param _doc The document object to process
 * @return true on success.
 */
    public static boolean start(RootDoc _doc) {
        doc = _doc; // Save _doc in a static field

// Read process any command line options
        readOptions(doc.options());

// Get a File object for the output file
        File f = new File(outputFileName);

// Assign ids to all of the classes to be processed.  They are stored in the hashtable
// keyed by their fully qualfied class name
        ClassDoc[] classes = doc.classes();
        classIds = new Hashtable(classes.length);
        for (int i=0;i<classes.length;i++) {
            classIds.put(classes[i].qualifiedName(), new Integer(ref++));
        }

// Open the output file
        try {
            out = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(outputFileName))));
        }
        catch (IOException exc) {
            doc.printError("Error opening output file");
            return false;
        }

// Write the Latex header information to the output file
        header();

// Get a list of all packages being processed
        PackageDoc[] packages = doc.specifiedPackages();

        int packageRef = 0;
// Generate the overview chapter listing all the packages
        if (packages.length > 0) {
            out.println("\\chapter{Overview}");
            out.println("\\label{res="+ref+"}");
            out.println("The following are all the packages contained in this document:");
            out.println("");
            packageListId = ref;    // Store the id of this window
            ref++;

            packageRef = ref;   // Store the id that refers to the package window
            ourPackages = new String[packages.length];  // An array of the package names of all the packages we are processing

// Write out a reference to each package window.  The ids of these will sequentially increment from packageRef.
            out.println("\\begin{itemize}");
            for (int i=0;i<packages.length;i++) {
                out.println("\\item\\ref{res="+ref+"} "+packages[i].name()+" (page \\   pageref{res="+ref+"})");
                ref++;
// Put the name of the package in ourPackages array
                ourPackages[i] = packages[i].name();
            }
            out.println("\\end{itemize}");

        }


// Process each package.
        if (packages.length > 0) {
            classes = null; // We don't need this list anymore so let it get garbage collected.
            for (int i=0;i<packages.length;i++) {
                processPackage(packages[i], packageRef+i);
                System.gc();    // Try and save some memory
            }
        }
        else {
            for (int i=0;i<classes.length;i++) {
                processClass(classes[i]);
            }
        }
// Write out the trailer
        trailer();
// Close the output file
        out.close();
        return true;
    }

/**
 * Writes the latex header.  In here we set our document class, load some extra packages,
 * set up the page headers, create the title page and table of contents, and so forth.
 */
    public static void header() {
        if (twoSide) {
            out.println("\\documentclass[twoside]{report}");
        }
        else {
            out.println("\\documentclass{report}");
        }
        out.println("\\usepackage{longtable}");
        out.println("\\usepackage{fancyhdr}");
        out.println("\\usepackage{makeidx}");
        out.println("\\title{"+title+"}");
        if (author.length() > 0) {
            out.println("\\author{"+author+"}");
        }
        Date d = new Date();

        out.println("\\date{"+DateFormat.getDateInstance().format(d)+"}");
        out.println("\\pagestyle{fancy}");
        out.println("\\renewcommand{\\chaptermark}[1]{\\markboth{#1}{}}");
        out.println("\\renewcommand{\\sectionmark}[1]{\\markright{\\thesection\\ #1}}");
        out.println("\\fancyhf{}");
        out.println("\\fancyhead[LE,RO]{\\bfseries\\thepage}");
        out.println("\\fancyhead[LO]{\\bfseries\\rightmark}");
        out.println("\\fancyhead[RE]{\\bfseries\\leftmark}");
        out.println("\\renewcommand{\\headrulewidth}{0.5pt}");
        out.println("\\renewcommand{\\footrulewidth}{0pt}");
        out.println("\\addtolength{\\headheight}{0.5pt}");
        out.println("\\fancypagestyle{plain}{%");
        out.println("    \\fancyhead{}");
        out.println("    \\renewcommand{\\headrulewidth}{0pt}");
        out.println("}");
        out.println("\\makeindex");
        out.println("\\begin{document}");
        out.println("\\maketitle");
        out.println("\\tableofcontents\\newpage");
    }


/**
 * Writes the trailer for the latex file.  Basically just prints the index
 * and the \end{document} tag.
 */
    public static void trailer() {
        out.println("\\printindex");
        out.println("\\end{document}");
    }



/**
 * Processes an individual package.  This means we start a new chapter and list all the classes in
 * the package, with references.  Then we detail each individual class in separate sections.
 * @param p The package to document
 * @param id The id to use for the label.  These have already been predetermined in the
 *           calling routine.
 */
    public static void processPackage(PackageDoc p, int id) {
        doc.printNotice("Processing package "+p.name());
// Write the chapter header.
        out.println("\\chapter{Package "+p.name()+"}");
        out.println("\\label{res="+id+"}");
        currentPackageId=id;

// Get all the classes in the package, and sort them using an ArrayList.
        ClassDoc[] classes=p.allClasses();
        ArrayList classArray = new ArrayList(classes.length);
        for (int i=0;i<classes.length;i++) {
            classArray.add(classes[i]);
        }
        Collections.sort(classArray);

// Copy sorted ArrayList back into a regular array.
        for (int i=0;i<classes.length;i++) {
            classes[i] = (ClassDoc) classArray.get(i);
        }

// Write out a reference to each class and interface.
        out.println("\\emph{Interfaces in package:}");
        out.println("");
        out.println("\\begin{itemize}");
        boolean atLeastOne = false;
        for (int i=0;i<classes.length;i++) {
            if (classes[i].isInterface()) {
                out.println("\\item\\ref{res="+getClassId(classes[i])+"} "+classes[i].name()+" (page \\pageref{res="+getClassId(classes[i])+"})");
                atLeastOne = true;
            }
        }
        if (!atLeastOne) {
            out.println("\\item None");
        }
        out.println("\\end{itemize}");
        out.println("");
        out.println("\\noindent\\emph{Classes in package:}");
        out.println("");
        out.println("\\begin{itemize}");
        atLeastOne = false;
        for (int i=0;i<classes.length;i++) {
            if (!classes[i].isInterface()) {
                out.println("\\item\\ref{res="+getClassId(classes[i])+"} "+classes[i].name()+" (page \\pageref{res="+getClassId(classes[i])+"})");
                atLeastOne = true;
            }
        }
        if (!atLeastOne) {
            out.println("\\item None");
        }
        out.println("\\end{itemize}");
        out.println("\\newpage");

// Process each class
        for (int i=0;i<classes.length;i++) {
            processClass(classes[i]);
        }
    }


/**
 * Gets the id for a given class.  Only works on classes that are going to be in the file we
 * are generating.  It finds it by looking it up in the hashtable <code>classIds</code>.
 * @param c The class to get the id for.
 * @return The id of the specified class.
 */
    public static int getClassId(ClassDoc c) {
        return getClassId(c.qualifiedName());
    }

/**
 * Gets the id for a given class.  Only works on classes that are going to be in the file we
 * are generating.  It finds it by looking it up in the hashtable <code>classIds</code>.
 * @param className The fully qualified class name to get the id for.
 * @return The id of the specified class.
 */
    public static int getClassId(String className) {
        Integer id = (Integer) classIds.get(className);
        if (id == null) {
            return 0;
        }
        return id.intValue();
    }

/**
 * Prevents latex from hyphenating the words, by appending a mandatory hyphen
 * point at the end of all the words in the string.  This process removes all
 * extra spaces.
 * @param str The string to de-hyphenate
 * @return The string with all words having \- appended
 */
    public static String dehyphenate(String str) {
        StringBuffer buf = new StringBuffer();
        StringTokenizer tok = new StringTokenizer(str);

        while (tok.hasMoreTokens()) {
            String token = tok.nextToken();
            buf.append(token);
// Appending a \- to the word keeps it from being hyphenated, but we only want to do this
// for relatively short words.  Latex likes to break words where they change from capital
// to small letter, so it usually does a good job of hyphenating long class names
            if (token.length() < 12) {
                buf.append("\\- ");
            }
        }
        return new String(buf);
    }

/**
 * Gets Latex for reference to specified class.  This produces a string containing a complete
 * Latex reference string, if the class is one of the ones we are processing.  Otherwise, it
 * just returns the name of the class.
 * @param className Fully qualified class name of the class to get the reference for
 * @param The text to be shown for the reference
 * @return String containing the reference text.
 */
    public static String getLink(String className, String text) {
// Try and lookup the class id for an internal link
        int id = getClassId(className);

// If the class could be found internally, then do a reference
        if (id != 0) {
            return text + " \\begin{sffamily}(see section \\ref{res="+id+"}, page=\\pageref{res="+id+"})\\end{sffamily}";
        }
        else {
            return text;
        }
    }


/**
 * Produces the latex for a particular class.  This consists of writing out the class section with
 * the description of the class, and field, constructor and method summaries, and then writing
 * out the text for each individual field, method, and constructor.
 * @param c The class to document
 */
    public static void processClass(ClassDoc c) {
// Write out the section header information.
        if ((c.isClass() && showClass) || c.isInterface())
            out.println("\\newpage");
        if ((c.isClass() && showClass) || c.isInterface())
            out.println("\\section{"+c.qualifiedName()+"}");
        else
            out.println("\\subsection{Implementing class: "+c.qualifiedName()+"}");
        out.println("\\label{res="+getClassId(c)+"}");
        if ((c.isClass() && showClass) || c.isInterface())
            out.println("\\index{"+processCommentText(c.name(),true)+"}");

// Print the superclass hierarchy for this class, followed by a black horizontal line.
        printClassTree(c);
        out.println("\\hrulefill");
        out.println("\\newline");

// Print out the modifiers for the class, followed by what class it extends, and what interfaces it implements
// (if any).
        out.println(c.modifiers()+" \\emph{"+c.name()+"}\\newline");
        if (c.superclass() != null) {
            out.println("extends "+c.superclass().name()+"\\newline");
        }
        ClassDoc[] interfaces = c.interfaces();
        if (interfaces.length > 0) {
            out.print("implements ");
            for (int i=0;i<interfaces.length;i++) {
                out.print(interfaces[i].name());
                if (i != interfaces.length-1) {
                    out.print(",");
                }
            }
            out.println("");
        }
        out.println("");
        out.println("");

// Print out the comment text.  Convert html tags to latex
        if ((c.isClass() && showClass) || c.isInterface())
            out.println(processCommentText(c.commentText(), false));
        out.println("");

// Write out any @see tags as references
        SeeTag[] seeTags = c.seeTags();
        if (seeTags.length > 0) {
            out.println("");
            out.println("\\textbf{See Also:}");
            out.println("");
            out.println("\\begin{itemize}");
            for (int i=0;i<seeTags.length;i++) {
                ClassDoc doc = seeTags[i].referencedClass();
                if (doc != null) {
                out.println("\\item "+getLink(doc.qualifiedName(), doc.name()));
                }
                else {
                    out.println("\\item "+seeTags[i].referencedClassName());
                }
            }
            out.println("\\end{itemize}");
        }

        if ((c.isClass() && showClass) || c.isInterface())
            {
// Get a list of all the fields, and sort it with an ArrayList
        FieldDoc[] fields = c.fields();
        ArrayList fieldArray=new ArrayList();
        for (int i=0;i<fields.length;i++) {
            fieldArray.add(fields[i]);
        }
        Collections.sort(fieldArray);
        for (int i=0;i<fields.length;i++) {
            fields[i] = (FieldDoc) fieldArray.get(i);
        }

// Write out the field summary table
        int fieldRef = ref;
        if (fields.length > 0 && showSummary) {
            out.println("\\subsection{Field Summary}");
            out.println("");
            out.println("\\begin{longtable}{|p{1in}|p{4in}|}");
            out.println("\\hline");
            out.println("\\textbf{Type} & \\textbf{Description} \\\\");
            out.println("\\hline\\hline");
            out.println("\\endhead");

            for (int i=0;i<fields.length;i++) {
                out.println("\\begin{raggedright}"+dehyphenate(fields[i].modifiers()+" "+fields[i].type().typeName()+fields[i].type().dimension()));
                out.println("");    // Need this blank line for raggedright to work properly
                out.println("\\end{raggedright} &");
                out.println("\\textbf{"+processCommentText(fields[i].name(), true)+"}");
                ref++;
                out.println("");
                Tag[] summary = fields[i].firstSentenceTags();
                boolean atLeastOne = false;
                for (int j=0;j<summary.length;j++) {
                    if (summary[j].kind().equalsIgnoreCase("text")) {
                        out.println(processCommentText(summary[j].text(), true)+"\\\\");
                        out.println("\\hline");
                        atLeastOne = true;
                    }
                }
                if (!atLeastOne) {
                    out.println("\\\\");
                    out.println("\\hline");
                }

            }
            out.println("\\end{longtable}");
        }

// Get a list of all the constructors.  Since they all have the same
// name, we don't sort this list.
        ConstructorDoc[] constructors = c.constructors();
        int constructorRef = ref;

// Write out the constructor summary table
        if (constructors.length > 0 && showSummary) {
            out.println("\\subsection{Constructor Summary}");
            out.println("");

            out.println("\\begin{longtable}{|p{5in}|}");
            out.println("\\hline");
            out.println("\\textbf{Description} \\\\");
            out.println("\\hline\\hline");
            out.println("\\endhead");

            for (int i=0;i<constructors.length;i++) {
                out.println("\\begin{raggedright}\\textbf{"+processCommentText(constructors[i].name()+getParamString(constructors[i]),true)+"}");
                ref++;
                out.println("");
                out.println("\\end{raggedright}");
                Tag[] summary = constructors[i].firstSentenceTags();
                boolean atLeastOne = false;
                for (int j=0;j<summary.length;j++) {
                    if (summary[j].kind().equalsIgnoreCase("text")) {
                        out.println(processCommentText(summary[j].text(),true)+"\\\\");
                        out.println("\\hline");
                        atLeastOne = true;
                    }
                }
                if (!atLeastOne) {
                    out.println("\\\\");
                    out.println("\\hline");
                }
            }
            out.println("\\end{longtable}");

        }

// Get a list of all the methods and sort it with an ArrayList
        int methodRef = ref;
        MethodDoc[] methods = c.methods();
        ArrayList methodArray=new ArrayList();
        for (int i=0;i<methods.length;i++) {
            methodArray.add(methods[i]);
        }
        Collections.sort(methodArray);
        for (int i=0;i<methods.length;i++) {
            methods[i] = (MethodDoc) methodArray.get(i);
        }

// Write out the method summary table
        if (methods.length > 0 && showSummary) {
            out.println("\\subsection{Method Summary}");
            out.println("");
            out.println("\\begin{longtable}{|p{1in}|p{4in}|}");
            out.println("\\hline");
            out.println("\\textbf{Returns} & \\textbf{Description} \\\\");
            out.println("\\hline\\hline");
            out.println("\\endhead");

            for (int i=0;i<methods.length;i++) {
                out.println("\\begin{raggedright}"+dehyphenate(methods[i].modifiers()+" " +methods[i].returnType().typeName()+methods[i].returnType().dimension()));
                out.println("");    // Need this blank line for raggedright to work properly
                out.println("\\end{raggedright} &");
                out.println("\\begin{raggedright}\\textbf{"+processCommentText(methods[i].name()+getParamString(methods[i]),true)+"}");
                ref++;
                out.println("");
                out.println("\\end{raggedright}");
                Tag[] summary = methods[i].firstSentenceTags();
                boolean atLeastOne = false;
                for (int j=0;j<summary.length;j++) {
                    if (summary[j].kind().equalsIgnoreCase("text")) {
                        out.println(processCommentText(summary[j].text(),true)+" \\\\");
                        out.println("\\hline");
                        atLeastOne = true;
                    }
                }
                if (!atLeastOne) {
                    out.println("\\\\");
                    out.println("\\hline");
                }
            }
            out.println("\\end{longtable}");

        }

        if (showDetails) {
// Output all the fields
            if (fields.length > 0) {
                processFields(fields, fieldRef);
            }
// Output all the constructors
            if (constructors.length > 0) {
                processConstructors(constructors,constructorRef);
            }
// Output all the methods
            if (methods.length > 0) {
                processMethods(methods,methodRef);
            }
        }
    }
    }


/**
 * Write out a the constructor details.  This method is called
 * by processClass to write out documentation for each constructor for the class.
 * @param docs Array of constructor information
 * @param startRes The reference id to use for the first constructor.  Add one for second constructor, and so forth.
 */
    public static void processConstructors(ConstructorDoc[] docs, int startRes) {
        out.println("\\subsection{Constructors}");

        for (int i=0;i<docs.length;i++) {
            startRes++;
            out.println("\\noindent");
            out.println("\\begin{sffamily}\\begin{large}");
            out.println("\\raisebox{1pt}{\\makebox[-1mm][l]{"+processCommentText(docs[i].name(), true)+"}}\\hrulefill");
            out.println("\\end{large}\\end{sffamily}");
            out.println("");
            out.println("\\begin{raggedright}");
            out.println("\\begin{em}"+processCommentText(docs[i].modifiers(),true)+" \\textbf{"+processCommentText(docs[i].name(),true)+"}"+getParamString(docs[i]));
            String exc = getExceptionsString(docs[i]);
            if (exc.length() > 0) {
                out.println("");
                out.println("throws "+exc);
            }
            out.println("\\end{em}");
            out.println("");
            out.println("\\end{raggedright}");
            out.println("\\vspace{1mm}");
            out.println(processCommentText(docs[i].commentText(),false));
            Parameter[] params = docs[i].parameters();
            out.println("\\vspace{3mm}");
            out.println("");
            int j;
            if (params.length > 0) {
                out.println("\\noindent\\textbf{Parameters:}");
                out.println("");
                out.println("\\begin{longtable}{|p{1.5in}|p{3.5in}|}");
                out.println("\\hline");
                ParamTag[] pTags = docs[i].paramTags();
                for (j=0;j<params.length;j++) {
                    out.println(processCommentText(params[j].name(), true)+" & ");
                    for (int l=0;l<pTags.length;l++) {
                        if (pTags[l].parameterName().equals(params[j].name())) {
                            out.print(processCommentText(pTags[l].parameterComment(),false));
                            break;
                        }
                    }
                    out.println(" \\\\");
                    out.println("\\hline");
                }
                out.println("\\end{longtable}");
            }
            out.println("");

// Exceptions
            ThrowsTag[] throwsTags = docs[i].throwsTags();
            if (throwsTags.length > 0) {
                out.println("\\noindent\\textbf{Throws:}");
                out.println("");
                out.println("\\begin{longtable}{p{1.5in} p{3.5in}}");
                for (j=0;j<throwsTags.length;j++) {
                    out.println(processCommentText(throwsTags[j].exceptionName(), true)+" &");
                    out.println(processCommentText(throwsTags[j].exceptionComment(),false)+" \\\\");
                }
                out.println("\\end{longtable}");
            }
            out.println("\\vspace{3mm}");
        }
    }


/**
 * Write out the method details.
 * @param docs Array of nethod information
 * @param startRes The reference id to use for the first method.  Add one for second method, and so forth.
 */
    public static void processMethods(MethodDoc[] docs, int startRes) {
        out.println("\\subsection{Methods}");

        for (int i=0;i<docs.length;i++) {
            startRes++;

            if (paramTable) {
                out.println("\\noindent");
                out.println("\\begin{sffamily}\\begin{large}");
                out.println("\\raisebox{1pt}{\\makebox[-1mm][l]{"+processCommentText(docs[i].name(), true)+"}}\\hrulefill");
                out.println("\\end{large}\\end{sffamily}");
            }

            out.println("");
            out.println("\\index{"+processCommentText(docs[i].name(), true)+"}");
            out.println("\\begin{raggedright}");
            Type t = docs[i].returnType();
            out.println("\\begin{em}"+processCommentText(docs[i].modifiers()+" "+t.typeName()+t.dimension(),true)+" \\textbf{"+processCommentText(docs[i].name(),true)+"}"+getParamString(docs[i]));
            String exc = getExceptionsString(docs[i]);
            if (exc.length() > 0) {
                out.println("");
                out.println("throws "+exc);
            }
            out.println("\\end{em}");
            out.println("");
            out.println("\\end{raggedright}");
            out.println("\\vspace{1mm}");
            out.println(processCommentText(docs[i].commentText(),false));
            Parameter[] params = docs[i].parameters();
            out.println("\\vspace{3mm}");
            out.println("");
            int j;
            if ((params.length > 0) && paramTable) {
                out.println("\\noindent\\textbf{Parameters:}");
                out.println("");
                out.println("\\begin{longtable}{|p{1.5in}|p{3.5in}|}");
                out.println("\\hline");
                ParamTag[] pTags = docs[i].paramTags();
                for (j=0;j<params.length;j++) {
                    out.println(processCommentText(params[j].name(), true)+" & ");
                    for (int l=0;l<pTags.length;l++) {
                        if (pTags[l].parameterName().equals(params[j].name())) {
                            out.print(processCommentText(pTags[l].parameterComment(),false));
                            break;
                        }
                    }
                    out.println(" \\\\");
                    out.println("\\hline");
                }
                out.println("\\end{longtable}");
            }
            out.println("");
// Do the return type
            Tag[] tags = docs[i].tags();
            for (j=0;j<tags.length;j++) {
                if (tags[j].name().equalsIgnoreCase("@return")) {
                    out.println("\\noindent\\textbf{Returns:}");
                    out.println("");
                    out.println(processCommentText(tags[j].text(),false));
                }
                out.println("");
            }
// Exceptions
            ThrowsTag[] throwsTags = docs[i].throwsTags();
            if (throwsTags.length > 0) {
                out.println("\\noindent\\textbf{Throws:}");
                out.println("");
                out.println("\\begin{longtable}{p{1.5in} p{3.5in}}");
                for (j=0;j<throwsTags.length;j++) {
                    out.println(processCommentText(throwsTags[j].exceptionName(), true)+" &");
                    out.println(processCommentText(throwsTags[j].exceptionComment(),false)+" \\\\");
                }
                out.println("\\end{longtable}");
            }
            out.println("\\vspace{3mm}");
        }
    }


/**
 * Write out a field details.
 * @param docs Array of field information
 * @param startRes The reference id to use for the first field.  Add one for second field, and so forth.
 */
    public static void processFields(FieldDoc[] docs, int startRes) {
        out.println("\\subsection{Fields}");

        for (int i=0;i<docs.length;i++) {
            startRes++;
            out.println("\\noindent");
            out.println("\\begin{sffamily}\\begin{large}");
            out.println("\\raisebox{1pt}{\\makebox[-1mm][l]{"+processCommentText(docs[i].name(), true)+"}}\\hrulefill");
            out.println("\\end{large}\\end{sffamily}");
            out.println("");
            out.println("\\index{"+processCommentText(docs[i].name(), true)+"}");
            out.println("\\begin{raggedright}");
            out.println("\\begin{em}"+processCommentText(docs[i].modifiers()+" "+docs[i].type().typeName()+docs[i].type().dimension(), true)+" \\textbf{"+processCommentText(docs[i].name(),true)+"}");
            out.println("\\end{em}");
            out.println("");
            out.println("\\end{raggedright}");
            out.println("\\vspace{1mm}");

            out.println(processCommentText(docs[i].commentText(),false));
            out.println("");
            out.println("\\vspace{3mm}");

        }
    }

/**
 * Outputs latex for superclass hierarchy for a class.
 * @param doc The class to print the chart for
 */
    public static void printClassTree(ClassDoc doc) {

// Build a list of all superclasses and then reverse it, since we want to show it in reverse order.
        ArrayList superClasses = new ArrayList();
        ClassDoc sClass = doc.superclass();
        if (sClass == null) return;
        while(sClass != null) {
            superClasses.add(sClass);
            sClass = sClass.superclass();
        }
        Collections.reverse(superClasses);

// Each level in indented 4 mm more than the previous one
        int indent=1;
        out.println("\\begin{enumerate}");
        for (int i=0;i<superClasses.size();i++) {
            sClass = (ClassDoc) superClasses.get(i);

            out.println("\\item[]\\hspace{"+indent+"mm}\\textsf{"+sClass.qualifiedName()+"}");

            indent += 4;
        }
        out.println("\\end{enumerate}");
    }

/**
 * Determines if the specified package is one being processed in this file.  If the package is not internal
 * then references to it cannot be done.
 * @param pack The package name to check for
 * @return true if the package is being documented in the latex file being generated.
 */
    public static boolean isPackageInternal(String pack) {
        for (int i=0;i<ourPackages.length;i++) {
            if (ourPackages[i].equals(pack)) {
                return true;
            }
        }
        return false;
    }

/**
 * Returns a string containing all the parameters for a method.  The parameters are separated by
 * commas and surrounded by brackets.
 * @param method The method to get the parameter list for
 * @return String with the parameter list
 */
    public static String getParamString(ExecutableMemberDoc method) {
        StringBuffer out = new StringBuffer("(");
        Parameter[] p = method.parameters();
        int i;
        if (p.length > 0) {
            for (i=0;i<p.length-1;i++) {
                out.append(p[i]+", ");
            }
            out.append(p[i]);
        }
        out.append(")");
        return processCommentText(new String(out), true);
    }

/**
 * Returns a string with a comma-separate list of all the exceptions that may be thrown by a method.
 * @param method The method to get the exception list for.
 * @return The comma-separated list of exceptions.
 */
    public static String getExceptionsString(ExecutableMemberDoc method) {
        StringBuffer out = new StringBuffer();
        ClassDoc[] exc = method.thrownExceptions();
        int i;
        if (exc.length > 0) {
            for (i=0;i<exc.length-1;i++) {
                out.append(exc[i].typeName()+", ");
            }
            out.append(exc[i].typeName());
        }
        return processCommentText(new String(out),true);
    }


/**
 * This method is called to convert certain characters in a string to their symbolic representation in
 * latex.  For example, you cannot leave a backslash in clear test, because it will be interpreted
 * as a latex command.  So, it is replaced with it's symbolic representation.  There are quite
 * a few special characters in Latex (good Unix-style design- use as many cryptic symbols as possible
 * in the syntax!) but I think this handles them all.
 * @param in The string to process
 * @return Processed string.
 */
    public static String processSymbols(String in) {
        StringBuffer out = new StringBuffer();
        char c;
        for (int i=0;i<in.length();i++) {
            c = in.charAt(i);
            if (c == '#') {
                out.append("\\#");
            }
            else if (c == '$') {
                out.append("\\$");
            }
            else if (c == '%') {
                out.append("\\%");
            }
            else if (c == '&') {
                out.append("\\&");
            }
            else if (c == '~') {
                out.append("\\verb+~+");
            }
            else if (c == '_') {
                out.append("\\_");
            }
            else if (c == '^') {
                out.append("\\verb+^+");
            }
            else if (c == '\\') {
                out.append("$\\backslash$");
            }
            else if (c == '{') {
                out.append("\\{");
            }
            else if (c == '}') {
                out.append("\\}");
            }
            else if (c == '\n') {
                out.append("\r\n");
                // Remove the space that follows
                i++;
            }
            else {
                out.append(c);
            }
        }
        return new String(out);
    }

/**
 * Process the comment or tag text to convert html tagging to Latex commands.  Sun, in particular, uses a lot
 * of html tags in its comments to format them nicely when converted to javadoc format.  Fortunately, many
 * of these can easily be easily converted to Latex.  Tags which are not recognized are omitted.  This is not
 * perfect, but usually what comes out is reasonably decent looking.  <code>processSymbols</code> is also
 * called by this method to fix up any tricky symbols.
 * @param in The text to process
 * @param inSummary If the text to be processed is from the summary, set this to true.  This prevents things
 * like lists from being translated, which sometimes causes problems for latex, because the end tag will
 * not be included because of truncation for the summary.
 * @return Cleaned up text.
 */
    public static String processCommentText(String in, boolean inSummary) {
        in = processSymbols(in);
        StringBuffer out = new StringBuffer();
        StringTokenizer tok = new StringTokenizer(in, "<>", true    );
        String token = null;
        boolean inTag = false;
        String flag = null;
        while (tok.hasMoreTokens()) {
            token = tok.nextToken();
            if (token.equals("<")) {
                if (!inTag) {
                    inTag = true;
                    flag = null;
                }
                else {
                    out.append(token);
                }
            }
            else if (token.equals(">")) {
                if (inTag) {
                    inTag = false;
                    if (flag != null) {
                        flag = flag.toLowerCase();
                        if (flag.equals("b")) {
                            out.append("\\begin{bfseries}");
                        }
                        else if (flag.equals("/b")) {
                            out.append("\\end{bfseries}");
                        }
                        else if (flag.equals("p")) {
                            out.append("\\par ");
                        }
                        else if (flag.equals("i")) {
                            out.append("\\begin{itshape}");
                        }
                        else if (flag.equals("/i")) {
                            out.append("\\end{itshape}");
                        }
                        else if (flag.equals("code")) {
                            out.append("\\begin{sffamily}");
                        }
                        else if (flag.equals("/code")) {
                            out.append("\\end{sffamily}");
                        }
                        else if (flag.equals("ul") || flag.startsWith("ul")) {
                            if (!inSummary) {
                                out.append("\\begin{itemize}");
                            }
                        }
                        else if (flag.equals("/ul")) {
                            if (!inSummary)
                                out.append("\\end{itemize}");
                        }
                        else if (flag.equals("ol") || flag.startsWith("ol")) {
                            if (!inSummary)
                                out.append("\\begin{enumerate}");
                        }
                        else if (flag.equals("/ol")) {
                            if (!inSummary)
                                out.append("\\end{enumerate}");
                        }
                        else if (flag.equals("li")) {
                            if (!inSummary)
                                out.append("\\item ");
                        }
                        else if (flag.equals("dl")) {
                            out.append("\\begin{longtable}{|p{1in}|p{4in}|}");
                            out.append("\\hline");
                            definitionListFirstLine = true;
                        }
                        else if (flag.equals("/dl")) {
                            out.append("\\\\\\hline\\end{longtable}");
                        }
                        else if (flag.equals("dt")) {
                            if (definitionListFirstLine) {
                                definitionListFirstLine = false;
                                out.append(" ");
                            }
                            else {
                                out.append("\\\\ \\hline ");
                            }
                        }
                        else if (flag.equals("dd")) {
                            out.append(" & ");
                        }
                        else if (flag.equals("hr")) {
                            out.append("\\hrulefill ");
                        }
                        else if (flag.equals("pre")) {
                            out.append("\\begin{verbatim}");
                        }
                        else if (flag.equals("/pre")) {
                            out.append("\\end{verbatim}");
                        }
                        flag = null;
                    }
                }
                else {
                    out.append(token);
                }
            }
            else {
                if (inTag) {
                    flag = token;
                }
                else {
                    out.append(token);
                }
            }

        }
        return (new String(out));
    }

/**
 * Returns how many option words specify a given tag.  This is part of the command-line option
 * processing for javadoc.  Any unregonized tags are passed in here, and it returns how
 * many symbols that tag should be.  For example, for -f which takes a filename as an argument,
 * it would return 2 (1 for the tag itself, and one for the filename).
 * @param option The option to get length for
 * @return The length for the option.
 */
    public static int optionLength(String option) {
        if (option.equals("-f")) {
            return 2;
        }
        else if (option.equals("-title")) {
            return 2;
        }
        else if (option.equals("-docauthor")) {
            return 2;
        }
        else if (option.equals("-nodetails")) {
            return 1;
        }
        else if (option.equals("-nosummary")) {
            return 1;
        }
        else if (option.equals("-twoside")) {
            return 1;
        }
        else if (option.equals("-noparamtable")) {
            return 1;
        }
        else if (option.equals("-noclass")) {
            return 1;
        }
        return 0;
    }

/**
 * Processes our extra command line options.  There are several extra command line options supported
 * by this doclet, and this scans through the options presented, and handles them appropriately.
 * No error checking is performed at this time.
 * @param options Array of options
 */
    public static void readOptions(String[][] options) {
        for (int i=0;i<options.length;i++) {
            String[] opt = options[i];
            if (opt[0].equals("-f")) {
                outputFileName = opt[1];
            }
            else if (opt[0].equals("-title")) {
                title=opt[1];
            }
            else if (opt[0].equals("-docauthor")) {
                author = opt[1];
            }
            else if (opt[0].equals("-nodetails")) {
                showDetails = false;
            }
            else if (opt[0].equals("-nosummary")) {
                showSummary = false;
            }
            else if (opt[0].equals("-twoside")) {
                twoSide = true;
            }
            else if (opt[0].equals("-noparamtable")) {
                paramTable = false;
            }
            else if (opt[0].equals("-noclass")) {
                showClass = false;
            }

        }
    }
}
