// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <javaobjc/Label.h>

@implementation Label

- createEnd
{
#if 0
  [self setClassIdFromName: "java/awt/Label"];
  [super createEnd];
#endif
  return self;
}

- setText: (const char *)t
{
  [self addString: t];
  return self;
}

@end

