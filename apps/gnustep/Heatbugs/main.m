#import <AppKit/AppKit.h>
#import <simtools.h>     // initSwarm () and swarmGUIMode
#include "HeatbugBatchSwarm.h"

#define APP_NAME @"Heatbugs GNUstep Application"

/*
 * Initialise and go!
 */

int main(int argc, const char *argv[]) 
{
  NSAutoreleasePool *pool;
  id theTopLevelSwarm;

  fprintf(stderr, "argc is %d\n", argc);
  if (argc >= 0)
      fprintf(stderr, "argv[0] %s\n", argv[0]);
  if (argc >= 1)
      fprintf(stderr, "argv[1] %s\n", argv[1]);

  initSwarm(argc, argv);
  fprintf(stderr, "after initSwarm\n");

  // swarmGUIMode is set in initSwarm(). It's set to be NO if you typed
  // `heatbugs --batchmode' or `heatbugs -b'. Otherwise, it's set to YES.
  if (swarmGUIMode == YES)
    {
      // We've got graphics, so make a full ObserverSwarm to get GUI objects
#if GNUSTEP
	theTopLevelSwarm = [HeatbugBatchSwarm createBegin: globalZone];
	theTopLevelSwarm = [theTopLevelSwarm createEnd];
#else
      theTopLevelSwarm = [HeatbugObserverSwarm createBegin: globalZone];
      SET_WINDOW_GEOMETRY_RECORD_NAME (theTopLevelSwarm);
      theTopLevelSwarm = [theTopLevelSwarm createEnd];
#endif
    }
  else
    // No graphics - make a batchmode swarm (using the key
    // `batchSwarm' from the default lispAppArchiver) and run it.
    if ((theTopLevelSwarm = [lispAppArchiver getWithZone: globalZone 
                                             key: "batchSwarm"]) == nil) 
      raiseEvent(InvalidOperation, 
                 "Can't find the parameters to create batchSwarm");

  [theTopLevelSwarm buildObjects];
  [theTopLevelSwarm buildActions];
  [theTopLevelSwarm activateIn: nil];
  [theTopLevelSwarm go];

  pool = [NSAutoreleasePool new];

  return NSApplicationMain (argc, argv);
}

