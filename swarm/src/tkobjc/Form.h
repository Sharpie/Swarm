// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// new widget: collects a bunch of label/entry pairs into a frame

#import <tkobjc/Widget.h>
#import <gui.h>

@interface Form: Widget <Form>
{
  unsigned entryCount;
  unsigned entryWidth;
}

- setEntryWidth: (unsigned)ew;
- addLineName: (const char *)n Boolean: (unsigned *)p;
- addLineName: (const char *)n Int: (int *)p;
- addLineName: (const char *)n Double: (double *)p;
@end
