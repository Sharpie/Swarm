// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Probe.h>
#import <objectbase.h>

@interface VarProbe: Probe <_VarProbe>
{
  const char *probedVariable;
  int dataOffset;
  BOOL interactiveFlag;
  const char *floatFormat; // actual sprintf-type format
}

- setProbedVariable: (const char *)aVariable;
- createEnd;

- setNonInteractive;
- setStringReturnType: returnType;
- setFloatFormat: (const char *)format;

- (const char *)getProbedVariable;
- (int)getDataOffset;
- (BOOL)getInteractiveFlag;

- setData: anObject To: (void *)newValue;	      // pass by reference.
- (BOOL)setData: anObject ToString: (const char *)s;  // gives us the string.

@end



