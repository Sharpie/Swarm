// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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
- setResultIdFlag: (BOOL)resultIdFlag;
+ createBegin: aZone;
- createEnd;

@end
