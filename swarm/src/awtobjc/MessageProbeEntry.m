// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/MessageProbeEntry.h>

@implementation MessageProbeEntry

- setArg: (int)theArg
{
  arg = theArg;
  return self;
}

- setIdFlag: (BOOL)theIdFlag
{
  idFlag = theIdFlag;
  return self;
}

+ createBegin: aZone
{
  MessageProbeEntry *obj = [super createBegin: aZone];
  
  obj->arg = -1;
  obj->idFlag = NO;
  return obj;
}
  
- createEnd
{
  BOOL argFlag = arg != -1;

  [super createEnd];
  if (idFlag)
    {
      [self setActiveFlag: NO];
      if (argFlag)
        {
          printf ("MessageProbeEntry:  with argFlag\n");
        }
      else
        {
          abort ();
        }
    }
  else
    abort ();
  
  [self packFillLeft: argFlag];

  return self;
}

@end

