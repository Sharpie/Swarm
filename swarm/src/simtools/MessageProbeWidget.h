// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc.h>
#import <objectbase/SwarmObject.h>
#import <objectbase/MessageProbe.h>

@interface MessageProbeWidget : Frame
{
  id myObject ;
  int argNum ;
  MessageProbe *myProbe ;
  Widget **myWidgets ;
  Entry *result ;
  int maxReturnWidth ;
  char resultType ;
  id resultObject ;
  int *objWindows ;
  char **obj_args ;
}

+ createBegin: aZone ;
- setObject: obj ;
- setProbe: (Probe *) the_probe;
- setMaxReturnWidth: (int) width ;
- createEnd ;
- update ;
- dynamic ;
- Spawn ;
- argSpawn: (int) which ;
- (const char *)getId ;
- (const char *)getId: (int) which ;
- (const char *)package ;
- (const char *)package: (int) which ;
- idReceive: (int) which ;
@end
