// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/MessageProbe.h>
#import <tkobjc.h>

// An object that fetches its data, and writes it into a file

@interface ActiveOutFile : MessageProbe {
  id theFile ;
  id dataFeed;
}

-setFileObject: aFileObj ;
-setDataFeed: d;
-step;
@end
