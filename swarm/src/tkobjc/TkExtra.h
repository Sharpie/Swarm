// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// TkExtra: add more initializations to Tk.h

#import <Tk.h>
#import <tk.h>

@interface TkExtra : Tk {
}
- (const char *)getBltVersion;
- (BOOL)newBLTp;
@end
