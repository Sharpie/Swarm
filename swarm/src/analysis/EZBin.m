// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis/EZBin.h>
#import <simtoolsgui.h>
#import <simtools.h> // OutFile

#include <misc.h> // xmalloc, XFREE
#include <math.h> // sqrt

#include <objc/objc-api.h>

#define NUMCOLORS 12

const char * const binColors[NUMCOLORS] =
//	{ "Red",     "Green",  "Yellow",    "Pink",      "SeaGreen",
//	  "Magenta", "Purple", "DarkGreen", "Goldenrod", "Black"     };
        { "Red",   "Blue",   "Orange", "DarkGreen", "Magenta",   "Purple",
	  "Green", "Yellow", "Cyan",   "SeaGreen",  "Goldenrod", "Black"  };

// const char * const binLabels[NUMCOLORS] = 
// 	{ "A","B","C","D","E","F","G","H","I","J","K","L" };

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
  return anObj;
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
  title = aTitle;
  if (!fileName)
    fileName = title;
  return self;
}

- setFileName: (const char *)aFileName
{
  fileName = aFileName;
  return self;
}

- setCollection: aCollection
{
  collection = aCollection;
  return self;
}

- setProbedSelector: (SEL) aSel
{
  probedSelector = aSel;
  return self;
}

- setBinNum: (int)theBinNum
{
  binNum = theBinNum;
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

  if (binNum <= 0)
    [InvalidCombination raiseEvent: "EZBin without a positive Bin Number!!!\n"];

  if (!title)
    [InvalidCombination raiseEvent: "EZBin without a title!!!\n"]; 

  if (fileOutput && !fileName)
    [InvalidCombination raiseEvent: "EZBin without an output-file name!\n"];

  if (min >= max)
    [InvalidCombination raiseEvent: "EZBin with invalid min-max range!!!\n"]; 

  [super createEnd];

  distribution = (int *)xmalloc (binNum * sizeof (int));
  cachedLimits = (double *)xmalloc (binNum * sizeof (double));
  locations = (double *)xmalloc (binNum * sizeof (double));
  step = (max - min) / ((double) binNum);

  for (i = 0; i < binNum; i++)
    {
      cachedLimits[i] = min + (((double) i) * step);
      locations[i] = min + 0.5 * step + (double)i * step;
    }
  [self reset];
  
  if (graphics)
    {
      aHisto = [Histogram createBegin: [self getZone]];
      [aHisto setNumBins: binNum];
      aHisto = [aHisto createEnd];
      SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME (aHisto);

      [aHisto setTitle: title];
      if(xLabel && yLabel) 
        [aHisto setAxisLabelsX: xLabel Y: yLabel];
      // [aHisto setLabels: binLabels];
      if (!monoColorBars)
        [aHisto setColors: binColors];
      [aHisto pack];
      
      [aHisto setBarWidth: step];
#if 0
      [aHisto setXaxisMin: min
              max: max
              step: 
                // stepsize cannot be same size than range
                ((binNum > 2)
                 ? (max - min) / (binNum - 1)
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
    anOutFile = [OutFile create: [self getZone] withName: fileName];
  
  return self;
}

PHASE(Using)

- setAxisLabelsX: (const char *)xl Y: (const char *)yl
{
  xLabel = xl;
  yLabel = yl;
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
  clean = 1;

  for (i = 0; i < binNum; i++)
    distribution[i] = 0;
  
  return self;
}

- update
{
  id iter, obj;
  char type0 = sel_get_type (sel_get_any_typed_uid (sel_get_name (probedSelector)))[0];

  iter = [collection begin: [self getZone]];
  while ((obj = [iter next]))
    {
      int i;
      double v;
      
      if (type0 == _C_DBL)
        v = (* ((double (*) (id, SEL, ...))[obj methodFor: probedSelector])) (obj, probedSelector);
      else if (type0 == _C_FLT)
        v = (double)(* ((float (*) (id, SEL, ...))[obj methodFor: probedSelector])) (obj, probedSelector);
      else
        v = (double)(* ((int (*) (id, SEL, ...))[obj methodFor: probedSelector])) (obj, probedSelector);
      
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
          clean = 0;
        }
      else
        {
          if (v < minval)
            minval = v;
          
          if (v > maxval)
            maxval = v;
          
          average = ((average * ((double)count)) + v)
            / ((double) (count + 1));
          
          average2 = ((average2 * ((double)count)) + v * v)
            / ((double) (count + 1));
          
          std = sqrt(average2 - average*average);
        }
      
      for (i = 0; i < binNum - 1; i++)
        if ((v >= cachedLimits[i]) && (v < cachedLimits[i + 1]))
          {
            distribution[i]++;
            break;
          }
      
      if (i == binNum - 1)
        distribution[i]++;
      
      count++;
    }
  
  [iter drop];
  return self;
}

- output
{
  int i;
  
  if (graphics)
    {
      [aHisto setActiveOutlierText: outliers count: count];
      [aHisto drawHistogramWithInt: distribution atLocations: locations];
    }
  
  if (fileOutput)
    {
      [anOutFile putInt: distribution[0]];
      for(i = 1; i < binNum; i++){
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
      for(i = 1; i < binNum; i++)
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

- (int *)getDistribution
{
  return distribution;
}

- (int)getCount
{
  return count;
}

- (int)getOutliers
{
  return outliers;
}

- (int)getBinNum
{
  return binNum;
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

- (double)getStd
{
  if (clean)
    [InvalidOperation
      raiseEvent:
        "Attempted to getStd from a reset EZBin (no data available).\n"];
  
  return std;
}

- (void)drop 
{
  XFREE (distribution);
  XFREE (cachedLimits);
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
