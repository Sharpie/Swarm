# This program invokes a Swarm application, after make-ing sure that the
# executable files are up to date.
#
# The program also converts application-specific command-line options to Java 
# properties for consumption by the application. Invoke this program with the 
# option --help to see what command-line options it supports. 
#
# You can specify command-line options in any order; for example, you can
# intermix general Swarm options such as "-b" with application-specific
# options.
#
# You can specify application-specific option arguments contiguously or 
# non-contiguously (e.g. -n50 or -n 50). 
#
# You can bundle application-specific options; only the last option in a bundle 
# may have an option argument (e.g. -picn50, equivalent to -p -i -c -n50 or 
# -p -i -c -n 50).
#
#
#
# The program extracts application-specific option specifications from the 
# documentation in the program named in the first argument. The extraction 
# logic assumes that those specifications are structured as in these examples:
#
#   <dir>
#   c=&lt;boolean&gt;: start the Heatbugs all in one cell
#   </dir>
#
#   <dir>
#   e=&lt;double&gt;:  specify the "evaporation" (really retention) rate (0..1)
#   </dir>

use FileHandle;

$swarmhome = $ENV{"SWARMHOME"};
$inname = @ARGV[0];
$true = 1;
$false = 0;

sub exit1
{
    $msg = shift;
    print        "stdout: $msg\n"; 
    print STDERR "stderr: $msg\n";
    exit 1;
}

sub println
{
    $msg = shift;
    print "$msg\n";
}

$dbg = $false;
sub debugprintln
{
    if ($dbg)
    {
        $msg = shift;
        print "$msg\n";
    }
}

$cmd = qq {$swarmhome/bin/make executable};
debugprintln "cmd is <$cmd>";
$result = system $cmd;
exit1 ("Exiting due to failure of make.") if ($result != 0);

$in = new FileHandle; 
open ($in, "$inname") || die ("$inname won't open: $!.");
($inclass = $inname) =~ s/\.java$//g;
undef $/;   # eliminate input record separator
$slurp = <$in>;

@matches = ($slurp =~ m/\<dir\>\s*(.*?)\s*\<\/dir\>/g);

foreach $match (@matches)
{
    debugprintln ("match is <$match>");
    ($doc = $match) =~ s/\s*/  -/;
    $doc =~ s/\&lt;/</;
    $doc =~ s/\&gt;:/>    /;
    $doc =~ m/^  -(\w)=\<(\w+)\>\s+.*/ 
     || exit1 ("Format of <$doc> is unexpected.");
    $opt = $1;
    $datatype = $2;
    debugprintln ("    doc is <$doc>");
    $$datatypeRH{$opt} = $datatype;
    debugprintln ("    datatype of $opt is $$datatypeRH{$opt}");
    $doc =~ s/^(  -\w)\=<boolean\>/$1          /;
    push (@$docRL, $doc);
}

debugprintln "\n";
# Start with $paramI = 1, because $ARGV[0] is $inname:
for ($paramI = 1; $paramI <= $#ARGV; $paramI++)
{
    $param = $ARGV[$paramI];
    debugprintln "param is <$param>";
    last if $param !~ m/^-/;
param:
    while ($param)
    {
debugprintln "checking $param";
        if ($param eq "--help" || $param eq "--usage" || $param eq "-?")
        {
            $help = $true;
        }
        foreach $key (keys %$datatypeRH)
        {
debugprintln "    checking $param against -$key";
            if ($param =~ m/^-$key/)
            {
                debugprintln "I matched $key with $param";
                if ($$datatypeRH{$key} eq "boolean")
                {
                    $propstring .= " -D$key=true";
                    debugprintln "with boolean, propstring is <$propstring>";
                    $param =~ s/^-./-/;
                    if ($param eq "-") { $param = ""; }
                    next param;
                }
                else
                {
                    if (length ($param) > 2)
                    {
                        $propstring .= " -D$key=" . substr ($param, 2);
                        debugprintln "with contiguous optarg, propstring is <$propstring>";
                        $param = "";
                        next param;
                    }
                    else
                    {
                        $propstring .= " -D$key=" . $ARGV[++$paramI];
                        debugprintln "with separate optarg, propstring is <$propstring>";
                        $param = "";
                    }
                }
            }
        }
        if ($param)
        {
            $otherparams .= " $param";
            $param = "";
            debugprintln "otherparams is <$otherparams>";
        }
    }
    next if $found;
}

$cmd = qq 
 {$swarmhome/bin/sh $swarmhome/bin/javaswarm $propstring $inclass $otherparams};
debugprintln "cmd is <$cmd>";
$result = system $cmd;
if ($help)
{
    print "\nApplication-specific options:\n\n";
    print (join ("\n", @$docRL));
    print "\n";
}
exit $result;
