
# This program invokes the jheatbugs application, through current.pl. Any
# options you specify on the command line are forwarded to and managed by
# current.pl.



# For reasons discussed below, now we're going to look for 
# $SWARMHOME/lib/perl*/*/File/Spec. We could use the -d check in the Cygwin 
# shell, but on some versions of Windows, Cygwin's -d check gives a wrong 
# answer. Status codes may also not work on those versions of Windows.
# So instead we will use ls to look for the directory.

rightspec=$SWARMHOME/lib/perl*/*/File/Spec/Unix.pm
lsoutput=$(ls $rightspec)
case $lsoutput in *Unix.pm)
        # Good - we found Unix.pm - so PERLLIB is fine. 
    ;;
    *)
    cat <<!

You seem to have no Swarm Perl directory named .../File/Spec. 

The non-existence of that directory seems to be due to an error in Swarm's 
installation of the Perl directories. Perl expects to use the library 
File/Spec/Unix.pm, but there is no such file in Swarm's Perl installation. 
This problem causes the following error message:

    Can't locate File/Spec/Unix.pm in @INC (@INC contains: ...)
    Compilation failed in require at 
     /Swarm-2.1.1/lib/perl5/5.00563/cygwin/IO/File.pm line 113.
    ...

The directory that Perl expects does exist, but in the Swarm distribution
it is named File/Specs rather than File/Spec. Swarm's Perl will work if and 
only if File/Specs is renamed to File/Spec (or if all the files in File/Specs 
are copied to File/Spec). 

To fix this problem:

Please rename (move) the Perl directory named .../File/Specs to .../File/Spec
(or copy all the files), and try this program again.

!
exit 1
    ;;
esac

# For PERLLIB, convert Windows directory names such as c:/xxx to //c/xxx:
SWARMHOME_FOR_PERLLIB=$(echo $SWARMHOME | sed 's,^\(.\):,//\1,')
echo SWARMHOME_FOR_PERLLIB is $SWARMHOME_FOR_PERLLIB

export PERLLIB="\
.\
:$SWARMHOME_FOR_PERLLIB/lib/perl5/5.00563\
:$SWARMHOME_FOR_PERLLIB/lib/perl5/5.00563/File/Spec\
"

$SWARMHOME/bin/perl current.pl StartHeatbugs.java "$@"
