// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase.h> // MessageProbe, val_t
#import <objectbase/Probe.h>

@interface MessageProbe: Probe <MessageProbe>
{
  SEL probedSelector;
  const char *probedMethodName;
  BOOL hideResultFlag;
  val_t *arguments;
}

- setProbedSelector: (SEL)aSel;
- setProbedMethodName: (const char *)methodName;
- createEnd;
+ create: aZone setProbedSelector: (SEL)aSel;

- (const char *)getProbedMessage;
- (unsigned)getArgCount;
- setArg: (unsigned)which ToUnsigned: (unsigned)val;
- setArg: (unsigned)which ToString: (const char *)str;
- (val_t)getArg: (unsigned)which;
- (const char *)getArgName: (unsigned)which;

- (val_t)dynamicCallOn: target;
- (double)doubleDynamicCallOn: target;
- (long)longDynamicCallOn: target;
- objectDynamicCallOn: target;
- (const char *)stringDynamicCallOn: target;

- (BOOL)isResultId;
- (BOOL)isArgumentId: (unsigned)which;

- (BOOL)getHideResult;
- setHideResult: (BOOL)hideResult;
- (void)describe: stream;
- (void)drop;
@end
