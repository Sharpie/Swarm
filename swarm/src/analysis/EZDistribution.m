// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis/EZDistribution.h>
#import <math.h>
#import <simtools.h> // OutFile
#import <gui.h>
#import <defobj/defalloc.h> // getZone macro

@implementation EZDistribution

PHASE(Creating)

- createEnd
{
  [super createEnd];

  probabilities =
    (double *) [getZone (self) alloc: binCount * sizeof (double)];

  maximumEntropy = log (1.0 / ((double) binCount));
   
  return self;
}

PHASE(Using)

- (void)update
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
}

- (void)output
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
  [getZone (self) free: probabilities];
  [super drop];
}

@end

