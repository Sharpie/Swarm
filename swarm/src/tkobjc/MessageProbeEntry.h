// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/Entry.h>
#import <gui.h>

@interface MessageProbeEntry: Entry <MessageProbeEntry>
{
  int arg;
  BOOL idFlag;
}

- setArg: (int)arg;
- setIdFlag: (BOOL)idFlag;
+ createBegin: aZone;
- createEnd;

@end
