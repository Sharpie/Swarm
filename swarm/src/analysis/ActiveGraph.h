// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis.h> // ActiveGraph
#import <objectbase/MessageProbe.h>
#import <gui.h>

// A graph that fetches its data, draws it on a GraphElement
@interface ActiveGraph: MessageProbe <ActiveGraph>
{
  id <GraphElement> element;			  // element to draw on
  id dataFeed;					  // object to read from
}

- setElement: (id <GraphElement>)ge;
- setDataFeed: d;
- step;
@end
