// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/Widget.h>

@interface Button: Widget
{
}

- setText: (const char *)text;		  // give the button a name
- setCommand: (const char *)command;	  // give the button a cmd
- setButtonTarget: target method: (SEL)sel;

@end
