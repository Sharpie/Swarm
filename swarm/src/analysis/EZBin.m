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

#import <analysis/EZBin.h>
#import <simtoolsgui.h>
#import <simtools.h> // OutFile
#import <defobj/defalloc.h> // getZone macro
#import <defobj.h> // STRDUP

#include <misc.h> // sqrt

#define NUMCOLORS 12

const char * const defaultBinColors[NUMCOLORS] =
	{ "blue", "orange", "yellow", "green",
	  "red", "purple", "violet", "cyan",
	  "grey50", "darkgreen", "goldenrod", "seagreen" };

@implementation EZBin

PHASE(Creating)

+ createBegin: aZone
{
  EZBin *anObj;
  
  anObj = [super createBegin: aZone];
  anObj->fileOutput = 0;
  anObj->graphics = 1;
  anObj->monoColorBars = NO;
  anObj->title = NULL;
  anObj->fileName = NULL;
  anObj->xLabel = NULL;
  anObj->yLabel = NULL;
  anObj->precision = 3;
  anObj->binColors = defaultBinColors;
  anObj->binColorCount = NUMCOLORS;

  return anObj;
}

- setColors: (const char * const *)colors count: (unsigned)nc
{
  binColorCount = nc;
  binColors = colors;

  return self;
}

- setGraphics: (BOOL)state
{
  graphics = state;
  return self;
}

- setFileOutput: (BOOL)state
{
  fileOutput = state;
  return self;
}

- setMonoColorBars: (BOOL)mcb
{
  monoColorBars = mcb;
  return self;
}

- setTitle: (const char *)aTitle
{
  title = STRDUP (aTitle);
  if (!fileName)
    fileName = title;
  return self;
}

- setFileName: (const char *)aFileName
{
  fileName = STRDUP (aFileName);
  return self;
}

- setCollection: aCollection
{
  collection = aCollection;
  return self;
}

- setProbedSelector: (SEL)aSel
{
  probedSelector = aSel;
  return self;
}

- setBinCount: (unsigned)theBinCount
{
  binCount = theBinCount;
  return self;
}

- setLowerBound: (double)theMin
{
  min = theMin;
  return self;
}

- setUpperBound: (double)theMax
{
  max = theMax;
  return self;
}

- createEnd
{
  unsigned i;
  double step;
  
  if (collection == nil)
    [InvalidCombination raiseEvent: "EZBin created without a collection\n"];

  if (binCount <= 0)
    [InvalidCombination raiseEvent: "EZBin without a positive Bin Number!!!\n"];

  if (!title)
    [InvalidCombination raiseEvent: "EZBin without a title!!!\n"]; 

  if (fileOutput && !fileName)
    [InvalidCombination raiseEvent: "EZBin without an output-file name!\n"];

  if (min >= max)
    [InvalidCombination raiseEvent: "EZBin with invalid min-max range!!!\n"]; 

  [super createEnd];

  distribution =
    (unsigned *) [getZone (self) alloc: binCount * sizeof (unsigned)];
  cachedLimits = (double *) [getZone (self) alloc: binCount * sizeof (double)];
  locations = (double *) [getZone (self) alloc: binCount * sizeof (double)];
  step = (max - min) / ((double) binCount);

  for (i = 0; i < binCount; i++)
    {
      cachedLimits[i] = min + (((double) i) * step);
      locations[i] = min + 0.5 * step + (double) i * step;
    }
  [self reset];
  
  if (graphics)
    {
      aHisto = [Histogram createBegin: getZone (self)];
      [aHisto setBinCount: binCount];
      SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME (aHisto);
      [aHisto setSaveSizeFlag: saveSizeFlag];
      aHisto = [aHisto createEnd];

      [aHisto setTitle: title];
      if(xLabel && yLabel) 
        [aHisto setAxisLabelsX: xLabel Y: yLabel];
      if (!monoColorBars)
        [aHisto setColors: binColors count: binColorCount];
      [aHisto pack];
      
      [aHisto setBarWidth: step];
#if 0
      [aHisto setXaxisMin: min
              max: max
              step: 
                // stepsize cannot be same size than range
                ((binCount > 2)
                 ? (max - min) / (binCount - 1)
                 : step)];
#else
      [aHisto setXaxisMin: min
              max: max
              step: step
              precision: precision];
#endif
      [aHisto setupZoomStack];
      [aHisto hideLegend];
      [aHisto setupActiveOutlierMarker];
      [aHisto setupActiveItemInfo];
    }
  
  if (fileOutput)
    anOutFile = [OutFile create: getZone (self) setName: fileName];
  
  return self;
}

- setAxisLabelsX: (const char *)xl Y: (const char *)yl
{
  xLabel = STRDUP (xl);
  yLabel = STRDUP (yl);
  return self;
}

- (void)setPrecision: (unsigned)thePrecision
{
  precision = thePrecision;
}

PHASE(Using)

- (void)reset
{
  unsigned i;
  
  count = 0;
  outliers = 0;
  clean = YES;

  for (i = 0; i < binCount; i++)
    distribution[i] = 0;
}

- (void)update
{
  if ([collection getCount] > 0)
    {
      id iter, obj;
      id <MessageProbe> mp = [[[[MessageProbe createBegin: scratchZone]
                                 setProbedClass: [[collection getFirst]
                                                   getClass]]
                                setProbedSelector: probedSelector]
                               createEnd];
      
      iter = [collection begin: getZone (self)];
      while ((obj = [iter next]))
        {
          unsigned i;
          double v = [mp doubleDynamicCallOn: obj];
          
          if (v > max || v < min)
            {
              outliers++;
              continue;
            }
          
          if (clean) 
            {
              maxval = v;
              minval = v;
              average = v;
              average2 = v * v;
              std = 0.0;
              clean = NO;
            }
          else
            {
              if (v < minval)
                minval = v;
              
              if (v > maxval)
                maxval = v;
              
              average = ((average * ((double) count)) + v)
                / ((double) (count + 1));
              
              average2 = ((average2 * ((double) count)) + v * v)
                / ((double) (count + 1));
              
              std = sqrt(average2 - average*average);
            }
          
          for (i = 0; i < binCount - 1; i++)
            if ((v >= cachedLimits[i]) && (v < cachedLimits[i + 1]))
              {
                distribution[i]++;
                break;
              }
          
          if (i == binCount - 1)
            distribution[i]++;
          
          count++;
        }
      
      [iter drop];
      [mp drop];
    }
}

- (void)output
{
  unsigned i;
  
  if (graphics)
    {
      [aHisto setActiveOutlierText: outliers count: count];
      [aHisto drawHistogramWithInt: distribution atLocations: locations];
    }
  
  if (fileOutput)
    {
      [anOutFile putInt: distribution[0]];
      for(i = 1; i < binCount; i++)
        {
          [anOutFile putTab];
          [anOutFile putInt: distribution[i]];
        }
      [anOutFile putNewLine];
    }
}

- (void)outputGraph 
{
  if (graphics)
    {
      [aHisto setActiveOutlierText: outliers count: count];
      [aHisto drawHistogramWithInt: distribution atLocations: locations];
    }
}

- (void)outputToFile 
{
  unsigned i;
  
  if (fileOutput)
    {
      [anOutFile putInt: distribution[0]];
      for(i = 1; i < binCount; i++)
        {
          [anOutFile putTab];
          [anOutFile putInt: distribution[i]];
        }
      [anOutFile putNewLine];
    }
}

- (const char *)getTitle
{
  return title;
}

- (const char *)getFileName
{
  return fileName;
}

- (unsigned *)getDistribution
{
  return distribution;
}

- (unsigned)getCount
{
  return count;
}

- (unsigned)getOutliers
{
  return outliers;
}

- (unsigned)getBinCount
{
  return binCount;
}

- (unsigned)getBinColorCount
{
  return binColorCount;
}

- (double)getLowerBound
{
  return min;
}

- (double)getUpperBound
{
  return max;
}

- (double)getMin
{
  if (clean)
    {
      [InvalidOperation 
        raiseEvent:
          "Attempted to getMin from a reset EZBin (no data available).\n"];
    }
  return minval;
}

- (double)getMax
{
  if (clean)
    [InvalidOperation
      raiseEvent:
        "Attempted to getMax from a reset EZBin (no data available).\n"];
  
  return maxval;
}

- (double)getAverage
{
  if (clean)
    [InvalidOperation
      raiseEvent:
        "Attempted to getAverage from a reset EZBin (no data available).\n"];
  return average;
}

- (double)getStdDev
{
  if (clean)
    [InvalidOperation
      raiseEvent:
        "Attempted to getStdDev from a reset EZBin (no data available).\n"];
  
  return std;
}

- (void)drop 
{
  [getZone (self) free: distribution];
  [getZone (self) free: cachedLimits];
  if (graphics)
    [aHisto drop];
  if (fileOutput)
    [anOutFile drop];
  
  [super drop];
}

- (id <Histogram>)getHistogram
{
  return aHisto;
}

@end
