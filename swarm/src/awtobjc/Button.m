// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/Button.h>

#include <objc/objc-api.h> // setButtonTarget

@implementation Button

- createEnd
{
#if 0
  [self setClassIdFromName: "java/awt/Button"];
  [super createEnd];
#endif
  return self;
}

- setText: (const char *)text
{
  [self addString: text];
  return self;
}

- setCommand: (const char *)command
{
  return self;
}

- setButtonTarget: target method: (SEL)sel
{
  return self;
}

@end

