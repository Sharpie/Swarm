// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Probe.h>

@interface VarProbe: Probe
{
  const char *probedVariable;
  int dataOffset;
  int interactive;
  const char *floatFormat; // actual sprintf-type format
}

- setProbedVariable: (const char *)aVariable;
- createEnd;

- setFloatFormat: (const char *)format;

- (const char *)getProbedVariable;
- (int)getDataOffset;

- setNonInteractive ;
- (int)isInteractive ;

- setData: anObject To: (void *)newValue;	     // pass by reference.
- (int)setData: anObject ToString: (const char *)s;  // gives us the string.

@end



