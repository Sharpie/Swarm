// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/Widget.h>
#import <gui.h>

@interface Button: Widget <Button>
{
}

- setText: (const char *)text;
- setButtonTarget: target method: (SEL)sel;

@end
