// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis.h>
#import <simtoolsgui.h>
#import <simtools.h> // OutFile

#include <stdlib.h> // free
#include <misc.h> // xmalloc
#include <math.h> // sqrt

#include <objc/objc-api.h>

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

//* The setGraphics method sets the state of the display. Set the state to 0 
//* if a graphical display of the graph is not required. The default state is 1
//* meaning that by default the data appears graphically in a window. 
- setGraphics: (int)state
{
  graphics = state;
  return self;
}

//* The setFileOutput method sets the state of file I/O.  Set the state to 1 
//* if data for the sequences is to be sent to a file.  The default state is 0
//* meaning that by default no file I/O is carried out by the EZBIN class.
- setFileOutput: (int)state
{
  fileOutput = state;
  return self;
}

//* The setTitle method uses a title string to label a graph window in the 
//* graphical version of EZBIN.  The label appears at the top of the graph 
//* window. (Only relevant if the state of setGraphics is set to 1.)
- setTitle: (const char *)aTitle
{
  theTitle = aTitle;
  return self;
}

//* The setAxisLabels:X:Y method sets the horizontal and vertical labels on 
//* the histogram in the graphical version of EZBIN. (Only relevant if the 
//* state of setGraphics is set to 1.)
- setAxisLabelsX: (const char *)xl Y: (const char *)yl
{
  xLabel = xl;
  yLabel = yl;
  return self;
}

//* The setBinNum method sets the number of bins the histogram will have.
- setBinNum: (int)theBinNum
{
  binNum = theBinNum;
  return self;
}

//* The setLowerBound method sets the lower bound on the histogram range.
- setLowerBound: (double)theMin
{
  min = theMin;
  return self;
}

//* The setUpperBound method sets the upper bound on the histogram range.
- setUpperBound: (double) theMax
{
  max = theMax;
  return self;
}

//* The setCollection method sets the collection of target objects which will 
//* be requested to generate the dataset for the histogram.
- setCollection: aCollection
{
  collection = aCollection;
  return self;
}

//* The setProbedSelector method sets the selector that will be applied to the 
//* objects in the specified collection in order to generate the dataset 
//* (inherited from MessageProbe.)
- setProbedSelector: (SEL) aSel
{
  probedSelector = aSel;
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

  distribution = (int *)xmalloc (binNum * sizeof(int));
  cachedLimits = (double *)xmalloc (binNum * sizeof(double));
  locations = (double *)xmalloc (binNum * sizeof(double));
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
      SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME (aHisto);
      aHisto = [aHisto createEnd];
      [aHisto setTitle: theTitle];
      if(xLabel && yLabel) 
        [aHisto setAxisLabelsX: xLabel Y: yLabel];
      [aHisto setNumPoints: binNum Labels: NULL Colors: NULL];
      [aHisto pack];
      
      [aHisto setBarWidth: step];
      [aHisto setXaxisMin: min
              max: max
              step: 
                // stepsize cannot be same size than range
                ((binNum > 2)
                 ? (max - min) / (binNum-1)
                 : (max-min) / binNum)];
      [aHisto setupZoomStack];
      [aHisto hideLegend];
      [aHisto setupActiveOutlierMarker];
      [aHisto setupActiveItemInfo];
    }
  
  if (fileOutput)
    anOutFile = [OutFile create: [self getZone] withName: theTitle];
  
  return self;
}

//* The reset method resets the histogram.
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

//* The update method polls the collection of objects and adds the data to the
//* final data set. It is possible to poll the same collection of objects 
//* repeatedly, thus increasing the amount of data included in the final 
//* dataset, before generating output.
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

//* The output method causes the graphical display to be updated with the 
//* information extracted by the previous call to update.  When file I/O is 
//* enabled (the state of setFileOutput is set to 1), the number of entries
//* per bin is sent to the output file. When the graphical display is enabled
//* (the state of setGraphics is set to 1), the histogram will be drawn.
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

//* The getDistribution method returns an array of integers containing the 
//* number of entries which landed in each bin of the histogram.
- (int *)getDistribution
{
  return distribution;
}

//* The getCount method gets the number of entries which landed within the 
//* bounds of the histogram.
- (int)getCount
{
  return count;
}

//* The getOutliers method gets the number of entries which landed out of the 
//* bounds of the histogram.  Pressing the "o" key on the graphical 
//* representation of the histogram will display this value both as an integer
//* and as a percentage of the total number of attempted entries.
- (int)getOutliers
{
  return outliers;
}

//* The getBinNum method gets the number of bins in the histogram.
- (int)getBinNum
{
  return binNum;
}

//* The getLowerBound method gets the lower bound on the histogram range.
- (double)getLowerBound
{
  return min;
}

//* The getUpperBound method gets the upper bound on the histogram range.
- (double)getUpperBound
{
  return max;
}

//* The getMin method gets the minimum value in the dataset.
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

//* The getMax method gets the maximum value in the dataset.
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

//* The getAverage method gets the average value in the dataset. The 
//* value is read out of the object, not computed everytime it is asked for.
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

//* The getStd method gets the standard deviation in the dataset. The 
//* value is read out of the object, not computed everytime it is asked for.
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
