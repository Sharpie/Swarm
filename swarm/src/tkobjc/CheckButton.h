// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Many thanks to JJ Merelo for helping with this widget.

#import <tkobjc/InputWidget.h>
#import <gui.h>

@interface CheckButton: InputWidget <CheckButton>
{
}

- (const char *)getValue;
- (BOOL)getBoolValue;
- setValue: (const char *)v;
- setBoolValue: (BOOL)v;
@end
