// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis.h> // FunctionGraph
#import <gui.h>
#import <objectbase/SwarmObject.h>

@interface FunctionGraph: SwarmObject <FunctionGraph>
{
   double minX;
   double maxX;
   double stepSize;

   id <GraphElement> element;	  // element to draw on

   id dataFeed;			  // object to read from
   SEL functionSEL;
   BOOL arithmeticWarn;

   unsigned resetFrequency;
   unsigned resetCountDown;
}

+ createBegin: aZone;
- createEnd;

- setElement          : (id <GraphElement>)e
           setDataFeed: f
   setFunctionSelector: (SEL)sel;

- setElement: (id <GraphElement>)graphElement;
- setDataFeed: feed;
- setFunctionSelector: (SEL)aSel;

- setArithmeticWarn: (BOOL)state;

- setXMin: (double)minx Max: (double)maxx Resolution: (unsigned)steps;
- setXMin: (double)minx Max: (double)maxx StepSize: (double)size;

- setResetFrequency: (unsigned)freq;

- graph;

@end


