// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// new widget: collects a bunch of label/entry pairs into a frame

#import <tkobjc/Frame.h>

@interface Form : Widget {
  int numEntries;
  int entryWidth;
}

-setEntryWidth: (int) ew;
-addLineName: (char *) n Variable: (void *) p Type: (int) type;

@end
