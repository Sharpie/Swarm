// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// new widget: collects a bunch of buttons into a frame, displays it.

#import <javaobjc/Frame.h>

@interface ButtonPanel: Frame
{
  const char *targetName;
}

- setButtonTarget: target;
- addButtonName: (const char *)n Command: (const char *)c;
- addButtonName: (const char *)n
     actionName: (const char *)action;

@end
