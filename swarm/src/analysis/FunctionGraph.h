#import <gui.h>
#import <objectbase/SwarmObject.h>

@interface FunctionGraph: SwarmObject
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

- setXMin: (double)minx Max: (double)maxx Resolution: (int)steps;
- setXMin: (double)minx Max: (double)maxx StepSize: (double)size;

- setResetFrequency: (unsigned)freq;

- graph;

@end


