// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/SuperButton.h>

@implementation SuperButton

- createEnd
{
  [super createEnd];

  if (superWidget)
    printf ("SuperButton configureWidgetToPackBeforeAndFillLeftThenDisableAndResize\n");
  else
    {
      printf ("SuperButton configureWidgetToBeep\n");
      [self setActiveFlag: NO];
    }
  
  return self;
}

- setSuperWidget: theSuperWidget
{
  superWidget = theSuperWidget;
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
