// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "Probe.h"

typedef int (*IntImp)(id, SEL, ...);
typedef float (*FloatImp)(id, SEL, ...);
typedef double (*DoubleImp)(id, SEL, ...);
typedef id (*IdImp)(id, SEL, ...);
typedef Class (*ClassImp)(id, SEL, ...);

@interface MessageProbe: Probe
{
  SEL probedSelector;
  int returnCategory;

  union {
    IntImp intImp;
    FloatImp floatImp;
    DoubleImp doubleImp;
    IdImp idImp;
    ClassImp classImp;
  } imp;
  int caching;
  
  const char *probedMessage;
  int argNum;
  int hr;
  const char **argLabels;
  const char **arguments;
}

- setProbedSelector: (SEL) aSel;
- setProbedMessage: (const char *)aMessage;
- createEnd;

- (const char *) getProbedMessage;
- (int) getArgNum;
- setArg: (int)which To: (const char *)what;
- setArg: (int)which ToObjectName: object;
- (const char *)getArg: (int) which;
- (const char *)getArgName: (int) which;

- setHideResult: (int)val;
- (int) getHideResult;

- _setImp_: anObject;
- updateMethodCache: anObject;

- dynamicCallOn: target resultStorage: (const char **)result;
- dynamicCallOn: target;
- (int)intDynamicCallOn: target;
- (float)floatDynamicCallOn: target;
- (double)doubleDynamicCallOn: target;

- (int)isResultId;
- (int)isArgumentId: (int) which;

@end
