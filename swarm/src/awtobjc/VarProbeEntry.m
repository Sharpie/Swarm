// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <javaobjc/VarProbeEntry.h>
#import <objc/objc-api.h>

@implementation VarProbeEntry

- setProbeType: (char)theProbeType
{
  probeType = theProbeType;
  return self;
}

- setInteractiveFlag: (BOOL)theInteractiveFlag
{
  interactiveFlag = theInteractiveFlag;
  return self;
}

- setOwner: theOwner
{
  owner = theOwner;
  return self;
}

- createEnd
{
  [super createEnd];

  if (interactiveFlag)
    {
      printf ("VarProbeEntry interactive\n");
    }
  else
    [self setActiveFlag: NO];

  if (probeType == _C_ID)
    {
      abort ();
    }
  else
    printf ("VarProbeEntry not id\n");
  return self;
}

@end

