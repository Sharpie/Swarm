// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase.h> // MessageProbe, val_t
#import <objectbase/Probe.h>

@interface MessageProbe: Probe <MessageProbe>
{
  SEL probedSelector;
  BOOL hideResultFlag;
  val_t *arguments;
}

- setProbedSelector: (SEL)aSel;
- createEnd;
+ create: aZone setProbedSelector: (SEL)aSel;

- (const char *)getProbedMessage;
- (int)getArgCount;
- setArg: (int)which ToString: (const char *)str;
- (val_t)getArg: (int)which;
- (const char *)getArgName: (int)which;

- (val_t)dynamicCallOn: target;
- (double)doubleDynamicCallOn: target;
- (long)longDynamicCallOn: target;
- objectDynamicCallOn: target;
- (const char *)stringDynamicCallOn: target;

- (BOOL)isResultId;
- (BOOL)isArgumentId: (int)which;

- (BOOL)getHideResult;
- setHideResult: (BOOL)hideResult;

@end
