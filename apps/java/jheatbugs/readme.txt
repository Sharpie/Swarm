

  		The jheatbugs sample Swarm application


This version of jheatbugs incorporates changes I made starting in the autumn of
2001.

I would appreciate any comment, question, suggestion, correction, or objection
that you would take the time to send me, at timothyrhowe@hotmail.com.

- Tim Howe


  		Section 1. Installing jheatbugs


You can find the jheatbugs sample Swarm application at swarm.org, under
Software - Applications. Or you can use wget to download it, as illustrated
below. 

You'll need to install Swarm itself, set the environment variable SWARMHOME
to the directory where you install Swarm, include $SWARMHOME/bin in your
PATH, and confirm the installation by invoking javaswarm --help. 

Here is one way to download and install jheatbugs:

    mkdir swarm
    cd swarm
    mkdir arc
    mkdir src
    cd arc
    wget ftp://ftp.swarm.org/pub/swarm/src/apps/java/jheatbugs-2.1.tar.gz
    cd ../src
    gzip -dc ../arc/*.tar.gz | tar xf -
    cd jheatbugs
    make executable


  		Section 1. Invoking jheatbugs


To run the current code of the application from Unix, or from a Unix-like
shell, or from DOS, invoke

    ksh current.ksh

for interactive mode, or

    ksh current.ksh -b

for batch mode, or

    ksh current.ksh --help

for help.

(On Cygwin you will probably need to type <tt>sh current.ksh</tt> rather than
<tt>ksh current.ksh</tt>).


  		Section 2. Goals of jheatbugs


This Java Swarm application is copiously documented, with the goal of providing
a useful tool for learning Swarm, for those who like to learn by studying
complete applications.

The application provides command-line arguments and some diagnostic output with
the goal of making experimentation easy. For example, invoke

    ksh current.ksh -c

and watch what happens when all the Heatbugs start their lives in a contiguous
cluster. Invoke

    ksh current.ksh -i

and watch what happens when the Heatbugs are immobile. Invoke

	ksh current.ksh -p 10

and observe the history of an arbitrary cell. Invoke

	ksh current.ksh -cip10

and guess how long it will take the heat to diffuse to the arbitrary cell for
which the history is reported.

Similarly for other command-line arguments, which you can list by invoking

    ksh current.ksh --help

Another goal is that this program should be not only an educational tool but
also a model for good programming: to that end, I have avoided sacrificing
quality of engineering for pedagogical purposes. Exceptions are generally
obvious or announced: for example, the quantity of comments, the presence of
methods that contain only diagnostic code, and ActionGroup and Action
structures that illustrate more possibilities than non-educational goals would
justify.


  		Section 3. Suggested learning path


1. Invoke the application, in batch and interactive mode, with various
combinations of options, to understand its black-box behavior.

2. Study just the class-level documentation in HeatbugModelSwarm.java.

3. Study Heatbug.java.

4. Study HeatSpace.java.

5. Study StartHeatbugs.java.

6. Study HeatbugModelSwarm.java.

7. Study HeatbugBatchSwarm.java.

8. Study HeatbugObserverSwarm.java.


  		Section 4. Naming and typographic conventions


On all private variable names, I use an underscore as a prefix.

To make programming structures obvious, I put almost every matching symbol --
that is, every symbol among ( ) { } [ ] < > /* */ -- either on the same line or
in the same column as its mate.

In documentation, I use the form "m()", with no space before the parentheses,
to mean "the method m, however many arguments it takes"; I use "m ()" to mean
"the method m, which takes one argument; "m (a)" to mean "the method m, which
takes one argument"; etc.

At the close of some of the longer methods, I put a comment so you can know
what method you're reading when the screen shows only the tail; for example, "}
/// buildObjects()". For constructors, the comment is "} /// constructor". For
the same reason, I sometimes put a comment at the end of a long loop; for
example, "} /// if _unhappiness != 0".

I generally begin a comment with "..." if it explains the preceding code; I end
it with ":" if it explains the subsequent code.

In the Javadoc comments, I indicate the role of every parameter with the string
"(in)" or "(out)" or "(inout)" (for example: "@param numBugs (in)") to
indicate, respectively, whether the parameter is only read or is only written
or is read and written by the method. Thus, if I pass an array to a method, and
the method or a delegate of the method might ever write an element of the array
and might ever also read an element of the array, I indicate "(inout)".

I define nearly every accessor (getter or setter) immediately after, and
indented from, its variable.

The fundamental idea of exception handling is to remove unusual conditions from
the normal flow of processing. Applying that concept to the typography, I do
not indent try-blocks.

Because most Java files contain exactly one top-level class, which all the
methods belong to, it is generally uninformative to indent each method
definition. Rather than squander one tab stop on a not very informative
typographic convention, I do not indent method definitions.
