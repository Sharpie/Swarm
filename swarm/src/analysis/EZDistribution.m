// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

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
      [anOutFile putDouble: probabilities[0]];
      for (i = 1; i < binCount; i++)
        {
          [anOutFile putTab];
          [anOutFile putDouble: probabilities[i]];
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

