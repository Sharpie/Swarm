// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/ClassDisplayHideButton.h>

@implementation ClassDisplayHideButton

- createEnd
{
  [super createEnd];

  if (subWidget)
    {
      abort ();
    }
  else
    {
      abort ();
    }

  return self;
}

- setSubWidget: theSubWidget
{
  subWidget = theSubWidget;
  return self;
}

- setOwner: theOwner
{
  owner = theOwner;
  return self;
}

- setUser: theUser
{
  user = theUser;
  return self;
}

@end

