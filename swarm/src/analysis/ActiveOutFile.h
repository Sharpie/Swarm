// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis.h> // ActiveOutFile
#import <objectbase/MessageProbe.h>

// An object that fetches its data, and writes it into a file

@interface ActiveOutFile: MessageProbe <ActiveOutFile>
{
  id theFile;
  id dataFeed;
}

- setFileObject: aFileObj;
- setDataFeed: d;
- step;
@end
