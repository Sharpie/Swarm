#!/usr/bin/perl -w
use strict;
use Getopt::Long;  # command line processing
# For File::Copy and IO::Dir, I needed to 
# export PERLLIB="$PERLLIB://c/Swarm-2.1.1/lib/perl5/5.00563":
use File::Copy; 
use IO::Dir; 

#Paul Johnson
# June 16, 2001

# WHAT IS THIS FOR?

# I just learned Perl, and because people having no Perl knowledge
# have difficulty writing their own scripts to manage parameter sweeps
# and repetition of simulations, I offer this!  It
# runs a program over and over and passes various command line
# options to it.  You can sweep through AS MANY PARAMETERS AS YOU WANT
# with AS MANY RUNS AS YOU WANT per setting.

# HOW DO I USE IT?

# You can give command line options or edit the CONFIGURATION
# section below.  Either way should end up the same.  Either way, you
# give the name of your program, the number of runs you want for each
# setting, and the parameters you want to sweep.  If you enter the
# values in the CONFIGURATION section below, you just get into a
# terminal and type

# perl replicator.pl 

# Otherwise, you pass command line options, as in (type all this on
# one line!)

# perl replicator.pl --program=rb --directory=/home/pauljohn/swarm/PJProjects 
# --NRUNS=3 --sweep numPPL=100 --sweep aRebConstPOM=0.1,0.2  --sweep vision=1,2,3

# And the thing should start working.

# This sets the numPPL variable at 100, and then two possible
# values for our variable aRebConstPOM, and then 3 values for 
# vision.  That means this script runs a total of 6 experiments,
# corresponding to these values:

# -vision=1 -aRebConstPOM=0.1 -numPPL=100
# -vision=1 -aRebConstPOM=0.2 -numPPL=100
# -vision=2 -aRebConstPOM=0.1 -numPPL=100
# -vision=2 -aRebConstPOM=0.2 -numPPL=100
# -vision=3 -aRebConstPOM=0.1 -numPPL=100
# -vision=3 -aRebConstPOM=0.2 -numPPL=100 

# Or if you use the "shortform" options to your program (short means
# one dash and no equal sign, as in -n100), and tell the script so
# with the shortform option:

# perl replicator.pl --program=rb --directory=/home/pauljohn/swarm/PJProjects 
#    --NRUNS=3 --shortform --sweep n=100  --sweep c=0.1,0.2  --sweep r=1,2,3

# In short or long form, the key is the "sweep" option, which you
# repeat for each parameter you want to examine. You can put one or
# more comma-separated options to sweep various combinations.
# Or you can specify a range; for example, 5..10.
# If you don't specify --directory, it looks in the current working
# directory.  If you don't specify NRUNS, it runs the program 1 time,
# or whatever number is set in the CONFIGURATION section below.

# WHY NOT USE DRONE?
# I love Drone for this kind of work, but Windows users around here
# have a hell of a time compiling expect, and we don't need all the drone
# features for networking and such, so we needed an alternative.  

# WHAT IS THE MEANING OF "EXPERIMENT" AND "RUN"

# I use the drone terminology here.  An "experiment" is a set of runs
# for a given set of parameters.  Every time you run this script, it
# runs at least one experiment.  A run is an execution of your program
# with a certain set of parameters.  You can have several runs of a
# program at a certain set of parameters, the only thing that changes
# is the random number seed that is fed into the program.

# Each time you run this script, by typing "perl runRB.pl", it should
# create a subdirectory exp-001, or if that exists, exp-002, and so forth.
# Inside there, you should see one directory for each parameter
# setting you select (see below).  You have the responsibility of
# creating output files with the run number in them if you want
# separate records for each run of the program.  It will create new 
# experiment directories every time it runs, but if your program
# writes on top of its old files when it repeats itself, it is 
# your own fault.

# This script will chdir to an experiment directory before it invokes 
# your program, so your program must be invocable from that directory.
# The script takes care of any SCM files by copying them from your invocation
# directory to the experiment directory. If your program is a Java
# program, you will probably need to list your invocation directory in
# $CLASSPATH. 

# HOW TO PREPARE YOUR SWARM PROGRAM
# The assumption is that you have a swarm batch program (though you can 
# use this script to run non-swarm programs as well) that runs with
# command line options. Suppose you can run a program by typing its
# name and a bunch of command line options, such as 

# ./rb -b --run=1 -seed=1234 --numPPL=110 --vision=2 --aRebConstPOM=0.1 

# or the short option form

# ./rb -b -R1 -S1234 -n110 -r2 -f0.1 

# -b is for batch, Swarm creates that flag. You create the rest within
# your Swarm arguments code.

# Any program you use with this script MUST accept at least 2 command
# line options (though it is free to ignore them):  

# 1. Run number.  Short form -Rx
#                  Long form --run=x

# 2. Seed Value.  Short form -Sx
#                  Long form --seed=x

# This script will take care of the seeds, replication, and so forth,
# but your program must accept the options. The script will
# create directories, and any output generated by your runs will be
# sorted into them.  You are rendered almost superfluous by its mighty
# power!  (not really...)

# You cannot use this script if you want to specify no-argument options.
# That's because GetOptions returns, for example, key z with value 1, whether 
# the command line contains "--sweep z" or "--sweep z=1". So there is no way 
# for this script to know that your program will accept -z but fail on -z1.

# Now, about the random number seeds.  I started the Perl random
# number generator with the number 1234321, you can change the seed
# number there if you want.  Then the script chooses random numbers
# from that stream as the seeds for following runs.


# *************CONFIGURATION*****************

# If you don't use command line options, HERE IS THE PART YOU EDIT:

# Give a full path to the directory that holds your program. 
# If you don't specify anything, or leave this blank, then the script
# assumes the directory is your current working directory. Even if 
# you are in MS-Windows, don't use separators like "\". Figure out 
# what your bash environment needs for paths by typing "pwd" in your
# terminal.

# Edit the following settings if you care to:

# Lets assume your program is in the current directory, the one where
# you plan to run this script:

my $dir_name = ".";

# That's not necessary, you can put your program wherever you want
# and tell the script so:
# my $dir_name = '/home/pauljohn/swarm/PJProjects/valinux/Protest';


# What is the name of your program:
my $prog_name= 'rb';

# How many runs do you want for each set of parameters.
my $NRUNS=1;

# Put in your command line parameters of interest here!  Here is an
# example, assuming you want to pass through the "long form" command
# line options.  Those are the ones like --option=value. Observe the
# structure of a line below. You need a command line parameter in long form
# (the kind that goes with -- in the command), the => symbol, and
# bracketed parameter values to be swept.  If you put just one value,
# it just uses that one value.  
my %parameters = (
           ### numPPL =>   [ 100 ],
           ### aRebConstPOM  =>   [0.1, 0.2],
           ### vision        =>   [1, 2, 3]
           );

# I personally like longform, but you may like shortform options.
# If you want shortform options, change this variable $shortform to 1:

#my $shortform = 0;
my $shortform = 1;

# and then give a %parameters statement like:
#my %parameters = (
#               n  =>   [ 100 ],
#               f  =>   [0.1, 0.2],
#               r  =>   [1, 2, 3]
#               );



# You don't really need to mess around below here. I think
# you should leave it alone, as a matter of fact.
###################################################




my @command_strings = "";

&main ();

sub main {
    &processCLI();
    
    foreach my $parameter ( keys %parameters ) {
    @command_strings = &updateCommandStrings( $parameters{$parameter}, $parameter ); 
    }

    my $expSuperDir = "";
    $expSuperDir = &createSuperDirectoryName();

    foreach my $string (@command_strings) {
    #beautify the string for directory creation purposes
    my $stringNoSpaces = $string;
    $stringNoSpaces =~ s/\ //g;
    $stringNoSpaces =~ s/--/-/g;
    my $fullPath = "$expSuperDir/$prog_name$stringNoSpaces";
    mkdir ($fullPath,0777);
    my $d = new IO::Dir "$dir_name"; 
    if (defined $d) { 
        while (defined (my $f = $d->read)) { 
            if ($f =~ m/scm$/i) { 
                File::Copy::copy ($f, $fullPath);
            }
        } 
    }
    chdir ($fullPath);
    
        if ($dir_name eq ".") { $dir_name = $ENV{PWD};}
    my $program = "$dir_name/$prog_name";

    open(COMMANDS, ">>experiment_summary");
    srand 1234321;
    my $seedValue = 0;

    for (my $i=0; $i < $NRUNS; $i++) {
         my $progString;
         $seedValue += int (rand 100000) + 1;
         if ($shortform ==1) {
         $progString = join("",$program," -b"," -R",$i," -S",$seedValue," ", $string);
         }
         else {
         $progString = join("",$program," -b"," --run=",$i," --seed=",$seedValue," ", $string);
         }
         
           print COMMANDS "$progString \n";

         print "Now trying to run = $progString \n";
         my $outputFile = "stdout$i";
         #diverts stderr to a file
         my $output = `$progString 2>&1 >> $outputFile`;
         print $output;
        
     }

    close(COMMANDS);
    chdir ($ENV{PWD});
    }
}


sub processCLI {
    my %clinput = ();

    GetOptions ('NRUNS:i' => \$NRUNS, 'directory:s' => \$dir_name, 'program:s' => \$prog_name , 'shortform' => \$shortform, "sweep=s" => \%clinput) || die "Invalid options \n";

    print "Your program is in directory: $dir_name \n";
    print "Your program names is: $prog_name \n";
    print "Your desired number of runs is: $NRUNS \n";
    print "Your command line input parameters: \n";
    if ($shortform == 1){print "You said you were using short form parameters "};

    foreach (keys %clinput) { print "$_ $clinput{$_} \n";}

    if (%clinput) {
    print "We are using your command line input for parameters \n";
    %parameters = ();
    foreach (keys %clinput) { 
        ## print "key is $_; value is $clinput{$_}\n";
        my @range_expansion;
        eval "\@range_expansion = ($clinput{$_})";
        $parameters{$_}=\@range_expansion;
    }
    }
}


sub updateCommandStrings {
  
    my @valArray = @{$_[0]};
    my $key = $_[1];
 

   # defined or $_ = " " foreach @{$local_strings};
   # @local_strings = @{$local_strings};

    
    my @oldStringArray = @command_strings;
    
    @command_strings=();
    
    foreach my $st (@oldStringArray){
    foreach my $val (@valArray){
        my $newString;
        if ($shortform == 0) {
        $newString = join("",$st," --",$key,"=",$val);
        }
        else {
        $newString = join("",$st," -",$key,$val);
        }
        push (@command_strings, $newString);
    }
    }
    return @command_strings;
}

sub createSuperDirectoryName {
    my $dirName = "exp-001";
    my $i=1;
  
    while (-e $dirName ){
    $i++;
    $dirName = join ("-","exp",sprintf("%03d",$i));
    }
    mkdir ($dirName,0777);
    return $dirName
}


exit;
