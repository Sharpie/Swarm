// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/Button.h>
#import <gui.h>

@interface ClassDisplayHideButton: Button <ClassDisplayHideButton>
{
  id owner;
  id user;
  id subWidget;
}

- setSubWidget: subWidget;
- setUser: user;
- setOwner: owner;
@end
