// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis.h> // EZBin
#import <simtoolsgui/GUIComposite.h>
#import <gui.h>

// EZBin object: used to generate histograms.

@interface EZBin: GUIComposite <EZBin>
{
  BOOL graphics;
  id <Histogram> aHisto;

  BOOL fileOutput;
  id anOutFile;

  BOOL monoColorBars;

  const char *fileName;
  const char *title;
  const char *xLabel;
  const char *yLabel;

  int *distribution;
  double *locations;
  double *cachedLimits;
  double min, max;
  int clean;
  int binNum;
  int count;
  int outliers;
  id collection;
  SEL probedSelector;

  double minval, maxval, average, average2, std;
  unsigned precision;
}

- setFileName: (const char *)aFileName;
- setTitle: (const char *)aTitle; 
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;

- setGraphics: (BOOL)state;
- setFileOutput: (BOOL)state;
- setMonoColorBars: (BOOL)mcb;

- setBinNum: (int)binNum;
- setLowerBound: (double)min;
- setUpperBound: (double)max;
- setCollection: (id)aCollection;
- setProbedSelector: (SEL)aSel;
- setPrecision: (unsigned)precision;

- reset;
- update;
- output;
- outputGraph;
- outputToFile;

- (int *)getDistribution;

- (unsigned)getCount;
- (int)getOutliers;
- (int)getBinNum;
- (double)getUpperBound;
- (double)getLowerBound;
- (const char *)getFileName;
- (const char *)getTitle;

- (double)getMin;
- (double)getMax;
- (double)getAverage;
- (double)getStd;

- (id <Histogram>)getHistogram;
@end
