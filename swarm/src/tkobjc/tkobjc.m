// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>
#import <tkobjc/TkExtra.h>

TkExtra * globalTkInterp;

id <Error> WindowCreation, WindowUsage;

void
initTkObjc(int argc, char ** argv) {
  deferror(WindowCreation, NULL);
  deferror(WindowUsage, NULL);
 
  globalTkInterp = [TkExtra alloc];
  [globalTkInterp initWithArgc: 1 argv: argv];
  [globalTkInterp registerObject: globalTkInterp withName: "globalTkInterp"];

  // (nelson) I think this is ok: lets us load cool graph code.
  // presumably, $blt_library is always set right.
  [globalTkInterp eval: "lappend auto_path $blt_library"];

  // (manor) this is used by EZBin in order to show info about contents 
  // of bins.

  [globalTkInterp eval:
     "proc active_item_info {graph} {
        $graph marker create text -coords { +Inf +Inf } \
          -name active_info_marker \
          -anchor ne\
          -bg {} -mapped 0

        uplevel #0 {set foundclosest 0} 

        bind $graph <Shift-ButtonPress-1> {

          if {[%W element closest %s %y closestretval -interpolate 1]} {
             set foundclosest 1
             %W marker configure active_info_marker \
             -text \"$closestretval(x),$closestretval(y)\" -mapped 1
             %W element activate $closestretval(name) $closestretval(index)
          }

        }

        bind $graph <ButtonRelease-1> {

             if {$foundclosest} {
               %W marker configure active_info_marker -mapped 0
               %W element deactivate $closestretval(name)
               set foundclosest 0 
             }
        }

        bind $graph <Enter> {
          focus %W
        }

        bind $graph <KeyPress-o> {
               %W marker configure active_outlier_marker -mapped 1
        }

        bind $graph <KeyRelease-o> {
               %W marker configure active_outlier_marker -mapped 0
        }
      }","%x"] ;
/*
proc Blt_ClosestPoint { graph } {
    bind bltClosestPoint <Control-ButtonPress-1>  {
        BltFindElement %W %x %y
        break
    }
    BltAddBindTag $graph bltClosestPoint 
}

proc BltFindElement { graph x y } {
    if ![$graph element closest $x $y info -interpolate 1] {
        bell
        return
    }
    # --------------------------------------------------------------
    # find(name)                - element Id
    # find(index)               - index of closest point
    # find(x) find(y)           - coordinates of closest point
    #                             or closest point on line segment.
    # find(dist)                - distance from sample coordinate
    # --------------------------------------------------------------
    set markerName "bltClosest_$info(name)"
    $graph marker delete $markerName
    BltFlashPoint $graph $info(name) $info(index) 10
}

  [globalTkInterp eval: 
     "proc label_bin_interval {min step window bin_num} {
        return [expr $min + ($step * $bin_num)] ;
      }"] ;
*/
}

