// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "Probe.h"
#import <objectbase.h>

@interface MessageProbe: Probe
{
  SEL probedSelector;
  BOOL hideResultFlag;
  val_t *arguments;
}

- setProbedSelector: (SEL)aSel;
- createEnd;

- (const char *)getProbedMessage;
- (int)getArgCount;
- setArg: (int)which ToString: (const char *)str;
- (val_t)getArg: (int)which;
- (const char *)getArgName: (int)which;

- (val_t)dynamicCallOn: target;
- (double)doubleDynamicCallOn: target;

- (BOOL)isResultId;
- (BOOL)isArgumentId: (int)which;

- (BOOL)getHideResult;
- setHideResult: (BOOL)hideResult;

@end
