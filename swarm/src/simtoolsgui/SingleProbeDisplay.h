// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/CommonProbeDisplay.h>

@interface SingleProbeDisplay: CommonProbeDisplay
{
  id probedObject;  
  BOOL removeRef;
  ref_t objectRef;
}

- setProbedObject: anObject;
- createEnd;

- getProbedObject;

- (void)setRemoveRef: (BOOL)removeRef;
- (void)setObjectRef: (ref_t)objectRef;

- (const char *)package: (const char *)windowName;
- (const char *)getId: (const char *)windowName;
@end
