// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Simple Graph Object -> encapsulates many of the low-level objects 
//                        required to get graphs/file traces to work.

#import <analysis.h> // EZGraph
#import <gui.h>
#import <simtoolsgui/GUIComposite.h>

@class EZGraph;
@class EZSequence;
@class EZAverageSequence;

@interface EZGraph: GUIComposite <EZGraph>
{
  int graphics;
  id <Graph> graph;

  BOOL fileOutput;
  id <HDF5> hdf5Container;
  id <HDF5> hdf5Group;
  id <HDF5> hdf5Dataset;

  id sequenceList;

  const char *xLabel;
  const char *yLabel;
  const char *title;
  const char *baseName;

  const char * const *graphColors;
  unsigned colorCount;

  unsigned colorIdx;
}
+ create: aZone setTitle: (const char *)aTitle setAxisLabelsX: (const char *)xl Y: (const char *)yl setWindowGeometryRecordName: (const char *)windowGeometryRecordName;
+ create: aZone setFileOutput: (BOOL)fileOutputFlag;
+ create: aZone setHDF5Container: (id <HDF5>)hdf5Container setPrefix: (const char *)prefix;
+ create: aZone setFileName: (const char *)aFileName;

- setGraphics: (BOOL)state;
- setFileOutput: (BOOL)state;

- setTitle: (const char *)aTitle;
- setFileName: (const char *)aFileName;
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;

- setColors: (const char * const *) colors count: (unsigned)nc;

- createEnd;

- (void)setRangesXMin: (double)xmin Max: (double)xmax;
- (void)setRangesYMin: (double)ymin Max: (double)ymax;
- (void)setScaleModeX: (BOOL)xs Y: (BOOL)ys;

- (id <Graph>) getGraph;

- createGraphSequence: (const char *)aName 
          forSequence: aSeq
         withFeedFrom: anObj 
          andSelector: (SEL)aSel;

- (id <EZSequence>)createSequence: (const char *)aName
    withFeedFrom: anObj 
     andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createAverageSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createMovingAverageSequence: (const char *)aName 
            withFeedFrom: aCollection
            andSelector: (SEL)aSel
               andWidth: (unsigned)width;

- (id <EZAverageSequence>)createVarianceSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createMovingVarianceSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel 
               andWidth: (unsigned)width;

- (id <EZAverageSequence>)createStdDevSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createMovingStdDevSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel 
               andWidth: (unsigned)width;

- (id <EZAverageSequence>)createTotalSequence: (const char *)aName 
         withFeedFrom: aCollection 
          andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createMinSequence: (const char *)aName
       withFeedFrom: aCollection 
        andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createMaxSequence: (const char *)aName
       withFeedFrom: aCollection 
        andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createCountSequence: (const char *)aName
         withFeedFrom: aCollection 
          andSelector: (SEL) aSel;

- dropSequence: aSeq;

- (const char *)getTitle;
- (const char *)getFileName;

- (void)step;

- (void)update;
- (void)outputGraph;
- (void)outputToFile;

@end

@interface EZSequence : SwarmObject <EZSequence>
{
  id <ActiveOutFile> activeOutFile;
  id <ActiveGraph> activeGrapher;
}

- (void)setActiveOutFile: (id <ActiveOutFile>)anActiveOutFile;
- (void)setActiveGrapher: (id <ActiveGraph>)aGrapher;
- (void)setUnsignedArg: (unsigned)val;

- (void)step;

- (void)update;
- (void)outputGraph;
- (void)outputToFile;

@end

@interface EZAverageSequence : EZSequence <EZAverageSequence>
{
  id <Averager> averager;
}

- (void)setAverager: (id <Averager>)anAverager;
- (void)setUnsignedArg: (unsigned)val;
@end

