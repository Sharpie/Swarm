// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <math.h>
#import <stdlib.h>
#import <collections.h>
#import <tkobjc.h>
#import <simtools.h>
#import <analysis.h>

#import <tkobjc/control.h>

@implementation EZBin

+ createBegin: aZone
{
  EZBin *anObj;
  
  anObj = [super createBegin: aZone];
  anObj->fileOutput = 0;
  anObj->graphics = 1;
  anObj->theTitle = NULL;
  anObj->xLabel = NULL;
  anObj->yLabel = NULL;
  return anObj;
}

- setWindowGeometryRecordName : (const char *)windowGeometryRecordName
{
  graphWindowGeometryRecordName = windowGeometryRecordName;
  return self;
} 

- setGraphics: (int)state
{
  graphics = state;
  return self;
}

- setFileOutput: (int)state
{
  fileOutput = state;
  return self;
}

- setTitle: (const char *)aTitle
{
  theTitle = aTitle;
  return self;
}

- setAxisLabelsX: (const char *)xl Y: (const char *)yl
{
  xLabel = xl;
  yLabel = yl;
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

- setUpperBound: (double) theMax
{
  max = theMax;
  return self;
}

- setCollection: aCollection
{
  collection = aCollection;
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

  if(!theTitle)
    [InvalidCombination raiseEvent: "EZBin without a title!!!\n"]; 

  if(min >= max)
    [InvalidCombination raiseEvent: "EZBin with invalid min-max range!!!\n"]; 

  [super createEnd];

  distribution = (int *) malloc (binNum * sizeof(int));
  cachedLimits = (double *) malloc (binNum * sizeof(double));
  locations = (double *) malloc (binNum * sizeof(double));
  step = (max - min) / ((double) binNum);

  for (i = 0; i < binNum; i++)
    {
      cachedLimits[i] = min + (((double) i) * step);
      locations[i] = min + 0.5*step + ((double)i)*step;
    }
  [self reset];

  if (graphics)
    {
      aHisto = [Histo createBegin: [self getZone]];
      [aHisto setWindowGeometryRecordName: graphWindowGeometryRecordName];
      aHisto = [aHisto createEnd];
      [aHisto title: theTitle];
      if(xLabel && yLabel) 
        [aHisto axisLabelsX: xLabel Y: yLabel];
      [aHisto setNumPoints: binNum Labels: NULL Colors: NULL];
      [aHisto pack];
      
      tkobjc_setHistogramBarWidth (aHisto, step);
      tkobjc_setHistogramXaxisRange (aHisto, min, max, 
                                     // stepsize cannot be same size than range
                                     ((binNum > 2)
                                      ? (max - min) / (binNum-1)
                                      : (max-min) / binNum));
      tkobjc_setupZoomStack (aHisto);
      tkobjc_setupHistogramLegend (aHisto);
      tkobjc_setupHistogramActiveOutlierMarker (aHisto);
      tkobjc_histogramActiveItemInfo (aHisto);      
    }
  
  if (fileOutput)
    anOutFile = [OutFile create: [self getZone] withName: theTitle];
  
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
  
  iter = [collection begin: [self getZone]];
  while ((obj = [iter next]))
    {
      int i;
      double v = [self doubleDynamicCallOn: obj];
      
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
          average2 = v*v;
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
          
          average2 = ((average2 * ((double)count)) + v*v)
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
      tkobjc_setHistogramActiveOutlierText (aHisto, outliers, count);
      [aHisto drawHistoWithInt: distribution atLocations: locations];
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
    {
      [InvalidOperation
        raiseEvent:
          "Attempted to getMax from a reset EZBin (no data available).\n"];
    }
  
  return maxval;
}

- (double)getAverage
{
  if (clean)
    {
      [InvalidOperation
        raiseEvent:
          "Attempted to getAverage from a reset EZBin (no data available).\n"];
    }
  return average;
}

- (double)getStd
{
  if (clean)
    {
      [InvalidOperation
        raiseEvent:
          "Attempted to getStd from a reset EZBin (no data available).\n"];
    }
  
  return std;
}

- (void)drop 
{
  free (distribution);
  free (cachedLimits);
  if (graphics)
    [aHisto drop];
  if (fileOutput)
    [anOutFile drop];
  
  [super drop];
}

@end
