#import <awtobjc/JavaObject.h>

typedef enum {
 JControlStateRunning = 0, 
 JControlStateStopped,
 JControlStateStepping, 
 JControlStateNextTime, 
 JControlStateQuit
} CtlState;

@interface JavaControlPanel: JavaObject
{
  jmethodID _getState;
  jmethodID _setState;
  jmethodID _waitRun;
  jmethodID _addAction;
}

+ createBegin: aZone;

// tell the control panel what actioncache to direct its signals to
- setActionCache: actionCache;
- addAction: (const char *)actName: actId;

//  set the control panel into a new state
- getState;
//  query the control panel state
- setState: state;

// wait for the control panel to signal non-stopped state
- (void)waitRun;

@end
