// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis/EZBin.h>
#import <simtoolsgui.h>
#import <simtools.h> // OutFile
#import <defobj/defalloc.h> // getZone macro
#import <defobj.h> // STRDUP

#include <misc.h> // sqrt

#include <objc/objc-api.h>

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
  int i;
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

PHASE(Using)

- setAxisLabelsX: (const char *)xl Y: (const char *)yl
{
  xLabel = STRDUP (xl);
  yLabel = STRDUP (yl);
  return self;
}

- setPrecision: (unsigned)thePrecision
{
  precision = thePrecision;
  return self;
}

- reset
{
  int i;
  
  count = 0;
  outliers = 0;
  clean = YES;

  for (i = 0; i < binCount; i++)
    distribution[i] = 0;
  
  return self;
}

- update
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
          int i;
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
  return self;
}

- output
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
  
  return self;
}

- outputGraph 
{
  if (graphics)
    {
      [aHisto setActiveOutlierText: outliers count: count];
      [aHisto drawHistogramWithInt: distribution atLocations: locations];
    }

  return self;
}

- outputToFile 
{
  int i;
  
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

  return self;
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
