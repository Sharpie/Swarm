// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/InputWidget.h>
#import <gui.h>

@interface Entry: InputWidget <Entry>
{
}

- createEnd;
- setValue: (const char *)t;
- (const char *)getValue;
- setHeight: (unsigned)h;

@end
