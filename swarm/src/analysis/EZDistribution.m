// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <math.h>
#import <stdlib.h>
#import <collections.h>
#import <simtools.h>
#import <analysis.h>
#import <gui.h>

#include <misc.h> // xmalloc

@implementation EZDistribution

- createEnd
{
  [super createEnd];

  probabilities = (double *)xmalloc (binNum * sizeof (double));
  maximumEntropy = log (1.0 / ((double) binNum));
   
  return self;
}

//* The update method polls the bins and updates the entropy of the 
//* distribution as well as the probabilities associated with the individual 
//* bins.
- update
{
  int i;

  [super update];

  for (i = 0; i < binNum; i++)
    {
      probabilities[i] = ((double)distribution[i]) / ((double)count);
      if (probabilities[i] > 0.0)
        entropy += probabilities[i] * log (probabilities[i]);
    }

  entropy /= maximumEntropy;

  return self;
}
//* The output method causes the graphical display to be updated with the 
//* information extracted by the previous call to update.  When file I/O is 
//* enabled (the state of setFileOutput is set to 1), the probability
//* associated with each bin is sent to the output file. When the graphical 
//* display is enabled (the state of setGraphics is set to 1), the histogram 
//* will be drawn.
- output
{
  int i;

  if (graphics)
    {
      [aHisto setActiveOutlierText: outliers count: count];
      [aHisto drawHistogramWithDouble: probabilities atLocations: locations];
    }

  if (fileOutput)
    {
      [anOutFile putInt: probabilities[0]];
      for (i = 1; i < binNum; i++)
        {
          [anOutFile putTab];
          [anOutFile putInt: probabilities[i]];
        }
      [anOutFile putNewLine];
    }
  
  return self;
}

//* The getProbabilities method returns an array of doubles representing
//* the probability of every bin in the distribution.
- (double *)getProbabilities
{
  if (clean)
    {
      [InvalidOperation 
        raiseEvent: 
          "Attempted to getProbabilities from a reset EZDistribution (no data available).\n"];
    }
  
  return probabilities;
}

//* The getEntropy method returns the entropy of the distribution as calculated
//* in the previous call to update.
- (double)getEntropy
{
  if (clean)
    {
      [InvalidOperation
        raiseEvent:
          "Attempted to getEntropy from a reset EZDistribution (no data available).\n"];
    }
  
  return entropy;
}


- (void)drop
{
  free (probabilities);
  [super drop];
}

@end

