// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/Probe.h>

@interface VarProbe: Probe {
  char * probedVariable;
  int dataOffset;
}

-setProbedVariable: (char *) aVariable;
-createEnd;

-(char *) getProbedVariable;
-(int)    getDataOffset;

-setData: anObject To: (void *) newValue;	  // pass by reference.
-setData: anObject ToString: (const char *) s;	  // gives us the string.
@end
