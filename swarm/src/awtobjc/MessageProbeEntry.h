// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/Entry.h>

@interface MessageProbeEntry: Entry
{
  int arg;
  BOOL resultIdFlag;
}

- setArg: (int)arg;
- setIdFlag: (BOOL)idFlag;
+ createBegin: aZone;
- createEnd;

@end
