// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/global.h>
#import <simtools/ProbeDisplay.h>
#import <simtools/CompleteProbeDisplay.h>
#import <simtools/ProbeDisplayManager.h>
#import <simtools/ControlPanel.h>
#import <swarmobject/probing.h>

#import <stdlib.h>
#import <time.h>
#import <activity.h>
#import <tkobjc.h>


id <PMMLCG1> randomGenerator;
id <UniformInteger> uniformIntRand;
id <UniformUnsigned> uniformUnsRand;
id <UniformDouble> uniformDblRand;
ProbeDisplayManager * probeDisplayManager;
int swarmGUIMode;

void
initSwarm(int argc, char ** argv) {
  int i ;

  initModule(activity);

  initProbing() ;

  randomGenerator = [PMMLCG1 createBegin: globalZone];
  [randomGenerator setStateFromSeed: 1234567890L ];
  randomGenerator = [randomGenerator createEnd];

  // Explicitly leave out the interval so that get{Type}WithMin:withMax:
  // can be used.
  uniformIntRand = [UniformInteger createBegin: globalZone];
  [uniformIntRand setGenerator: randomGenerator];
  uniformIntRand = [uniformIntRand createEnd];
  uniformUnsRand = [UniformUnsigned createBegin: globalZone];
  [uniformUnsRand setGenerator: randomGenerator];
  uniformUnsRand = [uniformUnsRand createEnd];
  uniformDblRand = [UniformDouble createBegin: globalZone];
  [uniformDblRand setGenerator: randomGenerator];
  uniformDblRand = [uniformDblRand createEnd];

  swarmGUIMode = 1;

  for(i = 1 ; i < argc ; i++)
    if( !strcmp(argv[i],"-batchmode") )
      swarmGUIMode = 0 ;
  
  if (swarmGUIMode) {
    initTkObjc(argc, argv);

  [globalTkInterp eval: "bitmap define hide {{16 16} {
   0x00, 0x00, 0x00, 0x00, 0x04, 0x80, 0x08, 0x40, 0x90, 0x27, 0x60, 0x18,
   0x60, 0x18, 0x90, 0x24, 0x10, 0x23, 0x10, 0x23, 0x90, 0x24, 0x60, 0x18,
   0x60, 0x18, 0x90, 0x27, 0x08, 0x40, 0x04, 0x80}}"] ;

  [globalTkInterp eval: "bitmap define super {{16 16} {
   0x00, 0x00, 0x00, 0x00, 0x7c, 0x08, 0x44, 0x04, 0x44, 0x7e, 0x44, 0x44,
   0x7c, 0x48, 0x00, 0x40, 0x00, 0x40, 0x00, 0x40, 0x00, 0x40, 0x7c, 0x40,
   0x44, 0x40, 0x44, 0x7e, 0x44, 0x00, 0x7c, 0x00}}"] ;

  [globalTkInterp eval: "bitmap define special {{16 16} {
   0x00, 0x00, 0x00, 0x00, 0x04, 0x80, 0x08, 0x40, 0x90, 0x27, 0x60, 0x1b,
   0x60, 0x1b, 0x90, 0x27, 0xf0, 0x3c, 0xf0, 0x3c, 0x90, 0x27, 0x60, 0x1b,
   0x60, 0x1b, 0x90, 0x27, 0x08, 0x40, 0x04, 0x80}}"] ;

  [globalTkInterp eval: "proc send_id {interp ddwin data} {
                           global DDOBJ ; set DDOBJ $data ;
                           drag&drop target $ddwin handle id }"] ;

  [globalTkInterp eval: "proc gimme {asdf} {
                           return $asdf }"] ;

  [globalTkInterp eval: "proc sitecmd {state token} {
 if {$state} {$token.l configure -fg OliveDrab} else {
              $token.l configure -fg black}}"] ;   

  [globalTkInterp eval: 
      "proc do_package {probe token} {
      set local [$probe package] ;
      if {$local != {}} {
        set label_text [$probe getId]  
      if {[winfo children $token] == {}} {
         label $token.l -text $label_text -fg black ; pack $token.l ;
      } else {
        $token.l config -text $label_text -fg black ;
      }
      return $local}}"] ;

  [globalTkInterp eval: 
      "proc do_package_arg {probe arg_num token} {
      set local [$probe package: $arg_num] ;
      if {$local != {}} {
        set label_text [$probe getId: $arg_num]  
      if {[winfo children $token] == {}} {
         label $token.l -text $label_text -fg black ; pack $token.l ;
      } else {
        $token.l config -text $label_text -fg black ;
      }
      return $local}}"] ;

  probeDisplayManager = [ProbeDisplayManager create: globalZone];
  } else {
    globalTkInterp = [Tcl alloc];		  // misnomer
    [globalTkInterp initWithArgc: 1 argv: argv];
    [globalTkInterp registerObject: globalTkInterp withName: "globalTkInterp"];
  }
  
  // various states used in ControlPanel.
  defsymbol(ControlStateRunning);
  defsymbol(ControlStateStopped);
  defsymbol(ControlStateStepping);
  defsymbol(ControlStateQuit);
  defsymbol(ControlStateNextTime);
}
