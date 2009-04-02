// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <Swarm/objectbase.h> // MessageProbe, val_t
#import <Swarm/Probe.h>

@interface MessageProbe: Probe <MessageProbe>
{
  SEL probedSelector;
  const char *probedMethodName;
  BOOL hideResultFlag;
  val_t *arguments;
  id <FCall> call;
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
