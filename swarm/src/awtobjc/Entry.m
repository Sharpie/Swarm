// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <javaobjc/Entry.h>

@implementation Entry

- createEnd
{
#if 0
  [self setClassIdFromName: "java/awt/TextField"];
  [super createEnd];
#endif
  return self;
}


- setValue: (const char *)t
{
  return self;
}

// ignore the height: doesn't work for entries.
- setWidth: (unsigned)w Height: (unsigned)h
{
  abort ();
  return self;
}

@end

