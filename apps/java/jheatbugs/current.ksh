# This program invokes the jheatbugs application, through current.pl. Any
# options you specify on the command line are forwarded to and managed by
# current.pl.

if [ ! -d $SWARMHOME/lib/perl*/*/File/Spec ]; then
    echo <<!

You seem to have no Swarm Perl directory named .../File/Spec. 

That seems to be due to an error in Swarm's installation of the Perl 
directories. 

If you have a Perl directory named .../File/Specs, please rename (move) it to
.../File/Spec and try this program again.

!
fi

export PERLLIB="\
.\
;$HOME/Swarm-2.1.1/lib/perl5/5.00563\
"

$SWARMHOME/bin/perl current.pl StartHeatbugs.java "$@"
