// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis/EZDistribution.h>
#import <math.h>
#import <simtools.h> // OutFile
#import <gui.h>

#include <misc.h> // xmalloc, XFREE

@implementation EZDistribution

PHASE(Creating)

- createEnd
{
  [super createEnd];

  probabilities = (double *)xmalloc (binCount * sizeof (double));
  maximumEntropy = log (1.0 / ((double) binCount));
   
  return self;
}

PHASE(Using)

- update
{
  unsigned i;

  [super update];

  for (i = 0; i < binCount; i++)
    {
      probabilities[i] = ((double)distribution[i]) / ((double)count);
      if (probabilities[i] > 0.0)
        entropy += probabilities[i] * log (probabilities[i]);
    }

  entropy /= maximumEntropy;

  return self;
}

- output
{
  unsigned i;

  if (graphics)
    {
      [aHisto setActiveOutlierText: outliers count: count];
      [aHisto drawHistogramWithDouble: probabilities atLocations: locations];
    }

  if (fileOutput)
    {
      [anOutFile putInt: probabilities[0]];
      for (i = 1; i < binCount; i++)
        {
          [anOutFile putTab];
          [anOutFile putInt: probabilities[i]];
        }
      [anOutFile putNewLine];
    }
  
  return self;
}

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
  XFREE (probabilities);
  [super drop];
}

@end

