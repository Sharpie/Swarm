// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdlib.h>
#import <time.h>
#import <string.h>

#import <objectbase/probing.h>
#import <simtools.h>
#import <activity.h>
#import <tkobjc.h>

#import <simtools/Archiver.h>

ProbeDisplayManager * probeDisplayManager;
int swarmGUIMode;
id applicationName, applicationMode;
void printHelp();

static void
setApplicationValue (id value, const char *ptr)
{
  const char *appStr = ptr;
  
  while (*ptr)
    {
      if (*ptr == '/')
        appStr = ptr + 1;
      ptr++;
    }
  [value setC: appStr];
}

void
initSwarm(int argc, char ** argv)
{
  int i ;

  initModule(activity);

  initProbing() ;

  swarmGUIMode = 1;
  applicationName = [String create: globalZone setC: ""];
  applicationMode = [String create: globalZone setC: ""];
  setApplicationValue (applicationName, argv[0]);
  setApplicationValue (applicationMode, "default");

  for(i = 1 ; i < argc ; i++) {
    if ( !strcmp(argv[i],"-help") )
      printHelp();
    else if ( !strcmp(argv[i],"-batchmode") )
      swarmGUIMode = 0 ;
    else if ( !strncmp(argv[i],"-a",2) && (i + 1 < argc))
      setApplicationValue (applicationMode, argv[i+1]);
  }
  
  archiver = [Archiver ensure : globalZone];

  initRandom(argc, argv);
  
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

void printHelp()
{
  (void) fprintf(stdout, "Swarm.  Copyright (C) 1997 Santa Fe Institute\n");
  (void) fprintf(stdout, "For more info, see:\n"
		 "http://www.santafe.edu/projects/swarm\n\n");
  (void) fprintf(stdout, "Supported command line flags are:\n\n");
  (void) fprintf(stdout, "\t  -appMode: Change the mode of the application\n");
  (void) fprintf(stdout, "\t-batchmode:  Run without a GUI\n");
  (void) fprintf(stdout, "\t -varySeed:  Change RandomSeed for each run\n");
  exit(-1);
}


