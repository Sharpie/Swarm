#!/usr/bin/perl

# jheatbugs-3.0

# Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
# This library is distributed without any warranty; without even the
# implied warranty of merchantability or fitness for a particular
# purpose.  See file COPYING for details and terms of copying.

# By Timothy Howe. 

# This program invokes a Swarm application. The program is written to be
# invoked by replicator.pl; always specify --program=repswarm.pl, though you
# may use a different path. 
#
# The program supports shortform and longform options. The program does not
# support bundled options nor spaces between an option and its option
# argument.
#
# If, for example, you specify the parameter
#
#		--sweep q=3..7
#
# to replicator.pl, that program will invoke this program (for the first run) 
# with the parameter
#
#		-q3
#
# and this program will invoke your Java Swarm program with the parameter
#
#		-Dq=3
#
# and your Java Swarm program can access the value in the standard fashion
# through the Java Property named "q".
#
# For boolean values, since this program does not support options without
# arguments, use the values 0 for false and 1 for true; for example,
#
#		--sweep c=0,1
#
# The program handles the Swarm batch option -b properly, regardless of where
# it appears among the application-specific parameters.  
#
# The program requires that the environment variable SWARMSTARTCLASS be defined 
# to indicate the initial Java class for javaswarm to invoke. 
#
# Note that replicator.pl handles numeric values only; for example, it can
# handle "--sweep x=173" or "--sweep scale=-0.5,-5,-50", but not "--sweep 
# c=R,G,B". That's why replicator.pl can't pass the Swarm start class name 
# through this program to the Swarm engine. 


$swarmhome = $ENV{"SWARMHOME"};

if (! $ENV{SWARMSTARTCLASS})
{
	print STDOUT "(stdout) Fatal error: SWARMSTARTCLASS is undefined; exiting.\n";
	print STDERR "(stderr) Fatal error: SWARMSTARTCLASS is undefined; exiting.\n";
	exit 1;
}

for ($paramI = 0; $paramI <= $#ARGV; $paramI++)
{
    $param = $ARGV[$paramI];
	if ($param !~ m/^-./ || $param eq "--")
	# ... "-" alone is not an option; "--" is the option-ending option. 
	{ last; }
	if ($param eq "-b")
    {
		$argstring .= "$param ";
	} elsif ($param =~ m/^--(.*?)=(.*)/)
    {
		$propstring .= "-D$1=$2 ";
	} elsif ($param =~ m/^--(.*)/)
    {
		$propstring .= "-D$1=1 ";
	} elsif ($param =~ m/^-(.)(.+)/)
    {
		$propstring .= "-D$1=$2 ";
	} else
    {
		$param =~ s/^-+//;
		$propstring .= "-D$param=1 ";
	}
}

for (; $paramI <= $#ARGV; $paramI++)
{
    $param = $ARGV[$paramI];
	$argstring .= " $param";
}

# Because this program is run by replicator.pl, the application's Java files
# are two directory levels up; hence we need to modify CLASSPATH. 
$cmd = qq 
 {CLASSPATH="../..:$ENV{CLASSPATH}" $swarmhome/bin/javaswarm $propstring $ENV{SWARMSTARTCLASS} $argstring};
print STDERR "In $0, invoking: $cmd ...\n";
$result = system $cmd;
exit $result;
